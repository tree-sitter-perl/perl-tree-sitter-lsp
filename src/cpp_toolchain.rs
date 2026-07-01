//! SPIKE: the zero-config C/C++ toolchain probe.
//!
//! `docs/cpp-stdlib-autoconfig-research.md` L2 — the load-bearing
//! layer. The `<...>` macro-gather in `cpp_reparse.rs` skips system
//! headers because it doesn't know where the stdlib lives; the only
//! portable, build-free source of that truth is the compiler itself.
//! Three one-shot invocations answer it, milliseconds each, stable per
//! toolchain, identical markers across gcc and clang on Linux + macOS:
//!
//!   1. `cc -x<lang> -E -v -`   → stderr's `#include <...>` search block
//!                                = ordered system/stdlib include roots.
//!   2. `cc -x<lang> -dM -E -`  → stdout's every predefined `#define`
//!                                = the EXTERNAL macro seed the gather's
//!                                  `#if` arms need (`__GNUC__`,
//!                                  `__cplusplus`, `_WIN32`, widths, …).
//!   3. `cc -print-resource-dir`→ clang's compiler-owned built-ins
//!                                (`stddef.h`, `stdarg.h`, intrinsics);
//!                                gcc has no such flag — those dirs
//!                                already surface in (1).
//!
//! Deliberately **not** wired into the gather yet — that consumption
//! (feed `predefined_macros` as the EXTERNAL seed, add `include_dirs`
//! to the `<...>` search) is a separate commit that would collide with
//! a concurrent reparse refactor. This module is the self-contained,
//! tested producer; `cpp_toolchain_tests.rs` exercises the parsers on
//! captured compiler output so the tests never depend on the host's
//! compiler.
//!
//! Caching: results are stable per (compiler abs-path, version, `-std`,
//! `--target`, lang) — a toolchain UPGRADE must invalidate this, a
//! project edit must NOT — so it lives in a process-local map keyed on
//! toolchain identity, separate from the per-project module cache. The
//! version rides the cached value so the persisted-blob follow-up (the
//! zstd machinery `PersistedMacros` already uses) can key its on-disk
//! entry on (abs-path, version, std, target) without a per-hit spawn.
//! MSVC's env-diff path (`vcvarsall.bat` → `INCLUDE`) is a follow-up.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::{Mutex, OnceLock};

/// Which language TU to probe. C++ adds the `c++/<ver>` roots and a
/// higher `__cplusplus`, so the search path and macro set differ — the
/// choice is part of the cache key, not a post-filter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Lang {
    C,
    Cpp,
}

impl Lang {
    /// The `-x` argument selecting the compile language.
    fn x_arg(self) -> &'static str {
        match self {
            Lang::C => "-xc",
            Lang::Cpp => "-xc++",
        }
    }
}

/// One toolchain's discovered stdlib surface. `include_dirs` is in
/// compiler search order (order is load-bearing — earlier wins) and
/// already includes `<resource_dir>/include` when that exists.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ToolchainInfo {
    /// System/stdlib include roots, in search order, deduped.
    pub include_dirs: Vec<PathBuf>,
    /// Every compiler-baked `#define`, as `(name, value)`; `value` is
    /// `""` for a bare `#define FOO`. Function-like macros keep their
    /// parameter list in `name` (`__has_feature(x)`).
    pub predefined_macros: Vec<(String, String)>,
    /// clang's `-print-resource-dir` (compiler-owned built-in headers).
    /// `None` for gcc, which has no such flag.
    pub resource_dir: Option<PathBuf>,
    /// First `--version` line — the toolchain identity component that a
    /// same-path upgrade changes; keys the persisted-blob follow-up.
    pub compiler_version: String,
}

/// The cache identity. `compiler` is the canonical absolute path when
/// resolvable (so `cc` and `/usr/bin/gcc` don't alias); `version` lives
/// on the cached value, recoverable with the other three for the
/// persisted key.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ToolchainKey {
    compiler: PathBuf,
    std: Option<String>,
    target: Option<String>,
    lang: Lang,
}

fn cache() -> &'static Mutex<HashMap<ToolchainKey, Option<ToolchainInfo>>> {
    static C: OnceLock<Mutex<HashMap<ToolchainKey, Option<ToolchainInfo>>>> = OnceLock::new();
    C.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Probe the default C++ driver's stdlib surface, cached. `std` is an
/// optional `-std=` value (e.g. `c++20`) forwarded so `__cplusplus` and
/// the `c++/<ver>` roots match the project. Returns `None` when no
/// compiler is on PATH — never panics; a machine may have none.
pub fn probe(compiler: &str, std: Option<&str>) -> Option<ToolchainInfo> {
    probe_full(compiler, Lang::Cpp, std, None)
}

/// Full-control probe: pick the language and forward an optional
/// `--target=` triple. Cached per (abs-path, version, std, target,
/// lang). Absence is cached too, so a missing compiler is probed once.
pub fn probe_full(
    compiler: &str,
    lang: Lang,
    std: Option<&str>,
    target: Option<&str>,
) -> Option<ToolchainInfo> {
    // Resolve to an absolute path so the key is toolchain identity, not
    // a PATH-relative alias. An unresolved name still keys (and probes)
    // by its raw spelling — `execvp` will fail cheaply and cache None.
    let resolved = resolve_compiler(compiler);
    let key = ToolchainKey {
        compiler: resolved.clone().unwrap_or_else(|| PathBuf::from(compiler)),
        std: std.map(str::to_string),
        target: target.map(str::to_string),
        lang,
    };

    if let Some(hit) = cache().lock().unwrap().get(&key).cloned() {
        return hit;
    }

    let driver = key.compiler.to_string_lossy().into_owned();
    let info = run_probe(&driver, lang, std, target);
    cache().lock().unwrap().insert(key, info.clone());
    info
}

/// The default driver for a language, honoring `$CC` / `$CXX` first,
/// then the conventional names in preference order. Returns the name to
/// pass to `probe*` (resolution to an abs path happens inside).
pub fn default_compiler(lang: Lang) -> Option<String> {
    let (env_var, candidates): (&str, &[&str]) = match lang {
        Lang::Cpp => ("CXX", &["c++", "clang++", "g++", "cc", "clang", "gcc"]),
        Lang::C => ("CC", &["cc", "clang", "gcc"]),
    };
    if let Ok(v) = std::env::var(env_var) {
        let v = v.trim();
        if !v.is_empty() {
            return Some(v.to_string());
        }
    }
    // `$CC` is the traditional override even for a C++ build's driver
    // discovery when `$CXX` is unset — check it as a secondary.
    if lang == Lang::Cpp {
        if let Ok(v) = std::env::var("CC") {
            let v = v.trim();
            if !v.is_empty() {
                return Some(v.to_string());
            }
        }
    }
    candidates
        .iter()
        .find(|c| resolve_compiler(c).is_some())
        .map(|c| c.to_string())
}

/// The three shell-outs, assembled. `None` iff the driver won't spawn
/// (no such compiler) — an empty include list from a driver that DID
/// run still yields `Some` (graceful, honest degradation).
fn run_probe(
    driver: &str,
    lang: Lang,
    std: Option<&str>,
    target: Option<&str>,
) -> Option<ToolchainInfo> {
    // `--version` doubles as the presence check and the identity stamp.
    let version = capture(driver, &["--version".to_string()])
        .map(|(out, _)| first_line(&out))?;

    let mut common: Vec<String> = vec![lang.x_arg().to_string()];
    if let Some(s) = std {
        common.push(format!("-std={s}"));
    }
    if let Some(t) = target {
        common.push(format!("--target={t}"));
    }

    // (1) system include roots — parse STDERR of `-E -v -` (empty stdin).
    let mut ev = common.clone();
    ev.extend(["-E".to_string(), "-v".to_string(), "-".to_string()]);
    let mut include_dirs = capture(driver, &ev)
        .map(|(_, err)| parse_include_search_dirs(&err))
        .unwrap_or_default();

    // (2) predefined macros — parse STDOUT of `-dM -E -`.
    let mut dm = common.clone();
    dm.extend(["-dM".to_string(), "-E".to_string(), "-".to_string()]);
    let predefined_macros = capture(driver, &dm)
        .map(|(out, _)| parse_predefined_macros(&out))
        .unwrap_or_default();

    // (3) clang resource dir — its `<dir>/include` holds compiler-owned
    // headers not in `/usr/include`. gcc lacks the flag (returns None).
    let resource_dir = capture(driver, &["-print-resource-dir".to_string()])
        .map(|(out, _)| first_line(&out))
        .map(PathBuf::from)
        .filter(|p| p.as_os_str().len() > 0 && p.is_dir());
    if let Some(rd) = &resource_dir {
        let builtin = rd.join("include");
        if builtin.is_dir() && !include_dirs.contains(&builtin) {
            include_dirs.push(builtin);
        }
    }

    Some(ToolchainInfo { include_dirs, predefined_macros, resource_dir, compiler_version: version })
}

/// Run `driver args…` with empty stdin; return `(stdout, stderr)` on a
/// successful spawn, `None` if the process couldn't start. A non-zero
/// exit is NOT an error here — `-print-resource-dir` on gcc exits
/// non-zero yet we still want the (empty) capture handled by the
/// caller's parse; what matters is whether `execvp` found the binary.
fn capture(driver: &str, args: &[String]) -> Option<(String, String)> {
    let out = Command::new(driver)
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .ok()?;
    Some((
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    ))
}

fn first_line(s: &str) -> String {
    s.lines().next().unwrap_or("").trim().to_string()
}

/// Slice the `#include <...> search starts here:` … `End of search
/// list.` block from `-E -v` stderr. gcc and clang emit identical
/// markers. Preserves order, trims leading space, strips clang's
/// macOS ` (framework directory)` suffix, dedups. The preceding
/// `#include "..."` (quote) block is intentionally excluded — we want
/// the angle-bracket/system roots.
pub fn parse_include_search_dirs(stderr: &str) -> Vec<PathBuf> {
    const START: &str = "#include <...> search starts here:";
    const END: &str = "End of search list.";
    let mut out: Vec<PathBuf> = Vec::new();
    let mut in_block = false;
    for line in stderr.lines() {
        let trimmed = line.trim();
        if !in_block {
            if trimmed == START {
                in_block = true;
            }
            continue;
        }
        if trimmed.starts_with(END) {
            break;
        }
        if trimmed.is_empty() {
            continue;
        }
        // clang tags framework roots; keep the path, drop the tag.
        let path = trimmed
            .strip_suffix(" (framework directory)")
            .unwrap_or(trimmed);
        let pb = PathBuf::from(path);
        if !out.contains(&pb) {
            out.push(pb);
        }
    }
    out
}

/// Split every `#define NAME VALUE` line from `-dM -E` stdout into
/// `(name, value)`. `NAME` is the token after `#define ` (function-like
/// macros keep their `(params)`); `VALUE` is the rest, or `""` for a
/// bare `#define NAME`. Order is preserved.
pub fn parse_predefined_macros(stdout: &str) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for line in stdout.lines() {
        let Some(rest) = line.trim().strip_prefix("#define ") else {
            continue;
        };
        let rest = rest.trim_start();
        match rest.split_once(char::is_whitespace) {
            Some((name, value)) => out.push((name.to_string(), value.trim().to_string())),
            None => out.push((rest.to_string(), String::new())),
        }
    }
    out
}

/// Resolve a compiler name to a canonical absolute path: an explicit
/// path is canonicalized; a bare name is searched on `$PATH`. `None`
/// when nothing executable matches.
fn resolve_compiler(name: &str) -> Option<PathBuf> {
    let p = Path::new(name);
    if p.components().count() > 1 || p.is_absolute() {
        return p.canonicalize().ok().filter(|c| c.is_file());
    }
    let path = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path) {
        let cand = dir.join(name);
        if cand.is_file() {
            if let Ok(abs) = cand.canonicalize() {
                return Some(abs);
            }
        }
    }
    None
}

#[cfg(test)]
#[path = "cpp_toolchain_tests.rs"]
mod tests;
