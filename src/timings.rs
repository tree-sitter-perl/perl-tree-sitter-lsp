//! Opt-in per-module build-timing instrumentation.
//!
//! Off by default: the gate is a single relaxed atomic load, so the hot
//! indexing/resolve paths pay nothing when timings aren't requested. When
//! enabled (via `--timings` on `--check` / `cli_full_startup`, or
//! `PERL_LSP_TIMINGS=1`), each module's parse + build wall time is recorded
//! into a global thread-safe collector and dumped slowest-first to stderr
//! after the index/check completes — so a cold-start outlier (the
//! `SQL::Abstract` blowup) is visible in one command.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;
use std::sync::OnceLock;
use std::time::Duration;

static ENABLED: AtomicBool = AtomicBool::new(false);

/// One module's timing breakdown. `cached` marks a module served from the
/// SQLite blob (no parse/build) vs freshly built.
struct Entry {
    module: String,
    parse: Duration,
    build: Duration,
    cached: bool,
}

fn collector() -> &'static Mutex<Vec<Entry>> {
    static COLLECTOR: OnceLock<Mutex<Vec<Entry>>> = OnceLock::new();
    COLLECTOR.get_or_init(|| Mutex::new(Vec::new()))
}

/// Turn instrumentation on. Honors an explicit flag OR `PERL_LSP_TIMINGS`.
/// Idempotent; safe to call from multiple CLI entry points.
pub fn enable() {
    ENABLED.store(true, Ordering::Relaxed);
}

/// Enable from environment if `PERL_LSP_TIMINGS` is set (any value).
pub fn enable_from_env() {
    if std::env::var_os("PERL_LSP_TIMINGS").is_some() {
        enable();
    }
}

#[inline]
pub fn is_enabled() -> bool {
    ENABLED.load(Ordering::Relaxed)
}

/// Record a freshly-built module's parse + build durations. Cheap no-op
/// when disabled (callers already gate the `Instant` capture on
/// `is_enabled()`, but this guards the lock too).
pub fn record_built(module: impl Into<String>, parse: Duration, build: Duration) {
    if !is_enabled() {
        return;
    }
    record(Entry {
        module: module.into(),
        parse,
        build,
        cached: false,
    });
}

/// Record a module served from the SQLite cache (no parse/build cost).
pub fn record_cached(module: impl Into<String>) {
    if !is_enabled() {
        return;
    }
    record(Entry {
        module: module.into(),
        parse: Duration::ZERO,
        build: Duration::ZERO,
        cached: true,
    });
}

fn record(e: Entry) {
    if let Ok(mut v) = collector().lock() {
        v.push(e);
    }
}

/// How many of the slowest entries to print in full. The rest are summarized.
const TOP_N: usize = 50;

/// Print the slowest-first breakdown to stderr. No-op when disabled or empty.
pub fn report() {
    if !is_enabled() {
        return;
    }
    let mut entries = match collector().lock() {
        Ok(mut v) => std::mem::take(&mut *v),
        Err(_) => return,
    };
    if entries.is_empty() {
        return;
    }

    entries.sort_by(|a, b| {
        let ta = a.parse + a.build;
        let tb = b.parse + b.build;
        tb.cmp(&ta)
    });

    let total: Duration = entries.iter().map(|e| e.parse + e.build).sum();
    let built = entries.iter().filter(|e| !e.cached).count();
    let cached = entries.len() - built;

    eprintln!();
    eprintln!(
        "=== per-module build timings ({} modules: {} built, {} cache-hit) ===",
        entries.len(),
        built,
        cached
    );
    eprintln!(
        "{:>10}  {:>10}  {:>10}  {:>6}  {}",
        "total_ms", "parse_ms", "build_ms", "source", "module"
    );

    for e in entries.iter().take(TOP_N) {
        let src = if e.cached { "cache" } else { "built" };
        eprintln!(
            "{:>10.3}  {:>10.3}  {:>10.3}  {:>6}  {}",
            (e.parse + e.build).as_secs_f64() * 1000.0,
            e.parse.as_secs_f64() * 1000.0,
            e.build.as_secs_f64() * 1000.0,
            src,
            e.module
        );
    }

    if entries.len() > TOP_N {
        eprintln!(
            "... {} more modules omitted (showing slowest {})",
            entries.len() - TOP_N,
            TOP_N
        );
    }
    eprintln!(
        "=== total build time across {} freshly-built modules: {:.3} ms ===",
        built,
        total.as_secs_f64() * 1000.0
    );
}
