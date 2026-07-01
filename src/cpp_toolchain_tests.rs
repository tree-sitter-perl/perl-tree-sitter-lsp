//! Parser tests run on CAPTURED compiler output (embedded below) so
//! they're deterministic on any host. One integration test actually
//! spawns a compiler, but is gated on one existing.

use super::*;

/// A real-shape `g++ -xc++ -E -v -` stderr capture (Debian, gcc 13):
/// two search blocks (quote then angle), the angle block carrying the
/// C++ `c++/13` roots ahead of the multiarch/system roots.
const GCC_EV_STDERR: &str = r#"Using built-in specs.
COLLECT_GCC=g++
Target: x86_64-linux-gnu
gcc version 13.2.0 (Debian 13.2.0-25)
ignoring nonexistent directory "/usr/local/include/x86_64-linux-gnu"
#include "..." search starts here:
 /some/project/local/quote/dir
#include <...> search starts here:
 /usr/include/c++/13
 /usr/include/x86_64-linux-gnu/c++/13
 /usr/include/c++/13/backward
 /usr/lib/gcc/x86_64-linux-gnu/13/include
 /usr/local/include
 /usr/include/x86_64-linux-gnu
 /usr/include
End of search list.
"#;

/// A macOS `clang -xc++ -E -v -` capture: an SDK path plus a framework
/// root tagged ` (framework directory)`.
const CLANG_EV_STDERR: &str = r#"clang version 17.0.6
#include "..." search starts here:
#include <...> search starts here:
 /Library/Developer/CommandLineTools/SDK/usr/include/c++/v1
 /usr/local/include
 /Library/Developer/CommandLineTools/usr/lib/clang/17/include
 /Library/Developer/CommandLineTools/SDK/usr/include
 /System/Library/Frameworks (framework directory)
End of search list.
"#;

/// A real-shape `-dM -E` capture: plain values, an `L`-suffixed value,
/// a value with spaces, a function-like macro, and a bare define.
const DM_STDOUT: &str = r#"#define __GNUC__ 13
#define __cplusplus 201703L
#define __x86_64__ 1
#define __SIZEOF_LONG__ 8
#define __STDC_HOSTED__ 1
#define _WIN32 1
#define __has_feature(x) 0
#define __FLT_MAX__ 3.40282347e+38F
#define __VERSION__ "13.2.0"
#define __STDC__
"#;

#[test]
fn include_dirs_slice_the_angle_block_in_order() {
    let dirs = parse_include_search_dirs(GCC_EV_STDERR);
    assert_eq!(
        dirs,
        vec![
            PathBuf::from("/usr/include/c++/13"),
            PathBuf::from("/usr/include/x86_64-linux-gnu/c++/13"),
            PathBuf::from("/usr/include/c++/13/backward"),
            PathBuf::from("/usr/lib/gcc/x86_64-linux-gnu/13/include"),
            PathBuf::from("/usr/local/include"),
            PathBuf::from("/usr/include/x86_64-linux-gnu"),
            PathBuf::from("/usr/include"),
        ]
    );
    // the quote-block dir must NOT leak into the system roots
    assert!(!dirs.contains(&PathBuf::from("/some/project/local/quote/dir")));
}

#[test]
fn include_dirs_strip_framework_tag() {
    let dirs = parse_include_search_dirs(CLANG_EV_STDERR);
    assert!(dirs.contains(&PathBuf::from("/System/Library/Frameworks")));
    assert!(!dirs.iter().any(|d| d.to_string_lossy().contains("framework directory")));
    assert_eq!(dirs.first().unwrap(), &PathBuf::from(
        "/Library/Developer/CommandLineTools/SDK/usr/include/c++/v1"
    ));
}

#[test]
fn include_dirs_empty_when_no_block() {
    assert!(parse_include_search_dirs("just some noise\nno markers here").is_empty());
}

#[test]
fn macros_split_name_and_value() {
    let m = parse_predefined_macros(DM_STDOUT);
    let get = |n: &str| m.iter().find(|(k, _)| k == n).map(|(_, v)| v.as_str());

    assert_eq!(get("__GNUC__"), Some("13"));
    assert_eq!(get("__cplusplus"), Some("201703L"));
    assert_eq!(get("_WIN32"), Some("1"));
    assert_eq!(get("__FLT_MAX__"), Some("3.40282347e+38F"));
    assert_eq!(get("__VERSION__"), Some("\"13.2.0\""));
    // function-like macro keeps its parameter list in the name
    assert_eq!(get("__has_feature(x)"), Some("0"));
    // bare define → empty value
    assert_eq!(get("__STDC__"), Some(""));
    // count matches the fixture's #define lines
    assert_eq!(m.len(), 10);
}

#[test]
fn macros_ignore_non_define_lines() {
    let out = "not a define\n#define A 1\n   # comment\n#define B\n";
    let m = parse_predefined_macros(out);
    assert_eq!(m, vec![("A".to_string(), "1".to_string()), ("B".to_string(), String::new())]);
}

#[test]
fn default_compiler_honors_env_override() {
    // A bogus $CXX is still returned verbatim (resolution happens later
    // in probe); this asserts the env is consulted first.
    let key = "CXX";
    let prev = std::env::var_os(key);
    std::env::set_var(key, "my-special-c++");
    assert_eq!(default_compiler(Lang::Cpp).as_deref(), Some("my-special-c++"));
    match prev {
        Some(v) => std::env::set_var(key, v),
        None => std::env::remove_var(key),
    }
}

/// If a real compiler is present, an end-to-end probe returns `Some`
/// with non-empty include dirs and a populated macro set. Gated so the
/// suite stays green on a compiler-less sandbox. Also asserts the cache
/// returns an equal result on the second call.
#[test]
fn integration_probe_real_compiler_when_present() {
    let Some(cc) = super::default_compiler(Lang::Cpp) else {
        eprintln!("no C++ compiler on PATH — skipping integration probe");
        return;
    };
    let Some(info) = probe(&cc, None) else {
        eprintln!("compiler {cc} did not spawn — skipping");
        return;
    };
    assert!(!info.include_dirs.is_empty(), "expected non-empty include dirs from {cc}");
    assert!(!info.predefined_macros.is_empty(), "expected predefined macros from {cc}");
    assert!(!info.compiler_version.is_empty());
    // __cplusplus is baked into every C++ toolchain.
    assert!(info.predefined_macros.iter().any(|(k, _)| k == "__cplusplus"));

    let again = probe(&cc, None).expect("cached second probe");
    assert_eq!(info, again);
}

/// A compiler that cannot exist resolves to nothing and probes to None,
/// without panicking.
#[test]
fn probe_absent_compiler_is_none() {
    assert_eq!(probe("definitely-not-a-real-compiler-xyz", None), None);
}
