//! SQLite persistence for the module cache (schema v9).
//!
//! Stores a full `Option<FileAnalysis>` per module, serialized via bincode
//! and compressed with zstd. Validates entries against mtime + file size to
//! detect stale data. Invalidates the entire cache when `@INC` changes.

use std::path::PathBuf;
use std::sync::Arc;
use std::time::SystemTime;

use dashmap::DashMap;
use rusqlite::{params, Connection};

use crate::file_analysis::FileAnalysis;
use crate::module_index::CachedModule;

const SCHEMA_VERSION: &str = "9";

/// Bumped when the builder's analysis output changes shape in a way that
/// invalidates cached blobs. Unlike `SCHEMA_VERSION`, this does not drop
/// the table — stale entries are re-resolved lazily with priority.
pub const EXTRACT_VERSION: i64 = 13;

/// zstd compression level for the `analysis` blob. Lower numbers are faster;
/// 3 is zstd's default and gives a solid space/speed tradeoff.
const ZSTD_LEVEL: i32 = 3;

pub fn cache_base_dir() -> Option<PathBuf> {
    if let Ok(xdg) = std::env::var("XDG_CACHE_HOME") {
        if !xdg.is_empty() {
            return Some(PathBuf::from(xdg).join("perl-lsp"));
        }
    }
    if let Ok(home) = std::env::var("HOME") {
        return Some(PathBuf::from(home).join(".cache").join("perl-lsp"));
    }
    None
}

pub fn cache_dir_for_workspace(workspace_root: Option<&str>) -> Option<PathBuf> {
    let base = cache_base_dir()?;
    match workspace_root {
        Some(root) => {
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};
            let mut hasher = DefaultHasher::new();
            root.hash(&mut hasher);
            Some(base.join(format!("{:016x}", hasher.finish())))
        }
        None => Some(base),
    }
}

#[cfg(not(test))]
pub fn open_cache_db(workspace_root: Option<&str>) -> Option<Connection> {
    let dir = cache_dir_for_workspace(workspace_root)?;
    std::fs::create_dir_all(&dir).ok()?;
    let db_path = dir.join("modules.db");
    log::info!("Module cache: {:?}", db_path);

    match Connection::open(&db_path) {
        Ok(conn) => {
            let _ = conn.execute_batch("PRAGMA journal_mode=WAL;");
            match init_schema(&conn) {
                Ok(()) => Some(conn),
                Err(e) => {
                    log::warn!("Cache DB schema init failed: {}. Recreating.", e);
                    drop(conn);
                    let _ = std::fs::remove_file(&db_path);
                    let conn = Connection::open(&db_path).ok()?;
                    let _ = conn.execute_batch("PRAGMA journal_mode=WAL;");
                    init_schema(&conn).ok()?;
                    Some(conn)
                }
            }
        }
        Err(e) => {
            log::warn!("Failed to open cache DB: {}", e);
            None
        }
    }
}

#[cfg(test)]
pub fn open_cache_db(_workspace_root: Option<&str>) -> Option<Connection> {
    None
}

pub fn init_schema(conn: &Connection) -> rusqlite::Result<()> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS meta (
            key   TEXT PRIMARY KEY,
            value TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS modules (
            module_name      TEXT PRIMARY KEY,
            path             TEXT NOT NULL,
            mtime_secs       INTEGER NOT NULL,
            file_size        INTEGER NOT NULL,
            source           TEXT NOT NULL DEFAULT 'import',
            analysis         BLOB,
            extract_version  INTEGER NOT NULL DEFAULT 0
        );",
    )?;

    let version: Option<String> = conn
        .query_row(
            "SELECT value FROM meta WHERE key = 'schema_version'",
            [],
            |row| row.get(0),
        )
        .ok();

    match version.as_deref() {
        Some(SCHEMA_VERSION) => Ok(()),
        Some(_) => {
            conn.execute_batch("DROP TABLE IF EXISTS modules;")?;
            conn.execute_batch(
                "CREATE TABLE modules (
                    module_name      TEXT PRIMARY KEY,
                    path             TEXT NOT NULL,
                    mtime_secs       INTEGER NOT NULL,
                    file_size        INTEGER NOT NULL,
                    source           TEXT NOT NULL DEFAULT 'import',
                    analysis         BLOB,
                    extract_version  INTEGER NOT NULL DEFAULT 0
                );",
            )?;
            conn.execute(
                "INSERT OR REPLACE INTO meta (key, value) VALUES ('schema_version', ?1)",
                params![SCHEMA_VERSION],
            )?;
            Ok(())
        }
        None => {
            conn.execute(
                "INSERT INTO meta (key, value) VALUES ('schema_version', ?1)",
                params![SCHEMA_VERSION],
            )?;
            Ok(())
        }
    }
}

pub fn compute_inc_hash(inc_paths: &[PathBuf]) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    for p in inc_paths {
        p.hash(&mut hasher);
    }
    format!("{:016x}", hasher.finish())
}

pub fn validate_inc_paths(conn: &Connection, inc_paths: &[PathBuf]) -> rusqlite::Result<()> {
    let current_hash = compute_inc_hash(inc_paths);
    let stored: Option<String> = conn
        .query_row(
            "SELECT value FROM meta WHERE key = 'inc_hash'",
            [],
            |row| row.get(0),
        )
        .ok();

    if stored.as_deref() != Some(&current_hash) {
        log::info!(
            "@INC changed (was {:?}, now {}), clearing module cache",
            stored,
            current_hash
        );
        conn.execute("DELETE FROM modules", [])?;
        conn.execute(
            "INSERT OR REPLACE INTO meta (key, value) VALUES ('inc_hash', ?1)",
            params![current_hash],
        )?;
    }
    Ok(())
}

/// Drop the module cache when the plugin set has changed since the last
/// run. `fingerprint` is the value returned by
/// `plugin::rhai_host::plugin_fingerprint()` — a hash over bundled
/// plugin sources plus every `.rhai` in `$PERL_LSP_PLUGIN_DIR`.
///
/// Without this check, a plugin author who edits a `.rhai`, restarts
/// the LSP, and inspects a cross-file query will see the *old*
/// plugin's emissions in the cached `FileAnalysis` blobs — making
/// plugin QA impossible. Mirrors `validate_inc_paths`: same meta-row
/// pattern, same hard-clear on mismatch.
pub fn validate_plugin_fingerprint(conn: &Connection, fingerprint: &str) -> rusqlite::Result<()> {
    let stored: Option<String> = conn
        .query_row(
            "SELECT value FROM meta WHERE key = 'plugin_fingerprint'",
            [],
            |row| row.get(0),
        )
        .ok();

    if stored.as_deref() != Some(fingerprint) {
        log::info!(
            "Plugin set changed (was {:?}, now {}), clearing module cache",
            stored,
            fingerprint
        );
        conn.execute("DELETE FROM modules", [])?;
        conn.execute(
            "INSERT OR REPLACE INTO meta (key, value) VALUES ('plugin_fingerprint', ?1)",
            params![fingerprint],
        )?;
    }
    Ok(())
}

fn mtime_as_secs(path: &std::path::Path) -> Option<(i64, i64)> {
    let meta = std::fs::metadata(path).ok()?;
    let mtime = meta.modified().ok()?;
    let secs = mtime.duration_since(SystemTime::UNIX_EPOCH).ok()?.as_secs() as i64;
    let size = meta.len() as i64;
    Some((secs, size))
}

/// Serialize FileAnalysis via bincode then compress with zstd.
fn encode_analysis(fa: &FileAnalysis) -> Option<Vec<u8>> {
    let bin = bincode::serialize(fa).ok()?;
    zstd::encode_all(bin.as_slice(), ZSTD_LEVEL).ok()
}

/// Decompress + deserialize an analysis blob.
fn decode_analysis(blob: &[u8]) -> Option<FileAnalysis> {
    let bin = zstd::decode_all(blob).ok()?;
    let mut fa: FileAnalysis = bincode::deserialize(&bin).ok()?;
    fa.after_deserialize();
    Some(fa)
}

pub fn warm_cache(
    conn: &Connection,
    cache: &DashMap<String, Option<Arc<CachedModule>>>,
) -> (usize, Vec<String>) {
    let mut stmt = match conn.prepare(
        "SELECT module_name, path, mtime_secs, file_size, analysis, extract_version FROM modules",
    ) {
        Ok(s) => s,
        Err(_) => return (0, Vec::new()),
    };

    let rows = match stmt.query_map([], |row| {
        Ok((
            row.get::<_, String>(0)?,
            row.get::<_, String>(1)?,
            row.get::<_, i64>(2)?,
            row.get::<_, i64>(3)?,
            row.get::<_, Option<Vec<u8>>>(4)?,
            row.get::<_, i64>(5)?,
        ))
    }) {
        Ok(r) => r,
        Err(_) => return (0, Vec::new()),
    };

    let mut count = 0usize;
    let mut stale_names = Vec::new();
    for row in rows.flatten() {
        let (module_name, path_str, cached_mtime, cached_size, analysis_blob, row_extract_version) = row;

        // Negative sentinel: empty path + NULL blob.
        if path_str.is_empty() {
            cache.insert(module_name, None);
            count += 1;
            continue;
        }

        let path = PathBuf::from(&path_str);

        // Validate mtime — skip entries where the file changed on disk.
        if let Some((disk_mtime, disk_size)) = mtime_as_secs(&path) {
            if disk_mtime != cached_mtime || disk_size != cached_size {
                continue;
            }
        } else {
            continue; // file deleted
        }

        // Check extract version — stale entries are still loaded but queued for re-resolve.
        if row_extract_version < EXTRACT_VERSION {
            stale_names.push(module_name.clone());
        }

        match analysis_blob {
            Some(blob) if !blob.is_empty() => {
                match decode_analysis(&blob) {
                    Some(fa) => {
                        cache.insert(
                            module_name,
                            Some(Arc::new(CachedModule::new(path, Arc::new(fa)))),
                        );
                        count += 1;
                    }
                    None => {
                        log::warn!("Failed to decode cached analysis for '{}', skipping", module_name);
                    }
                }
            }
            _ => {
                // Blob missing / empty — treat as negative sentinel.
                cache.insert(module_name, None);
                count += 1;
            }
        }
    }

    (count, stale_names)
}

pub fn save_to_db(
    conn: &Connection,
    module_name: &str,
    result: &Option<Arc<CachedModule>>,
    source: &str,
) {
    let (path_str, mtime, size, analysis_blob) = match result {
        Some(cached) => {
            let (mtime, size) = mtime_as_secs(&cached.path).unwrap_or((0, 0));
            let blob = encode_analysis(&cached.analysis);
            (cached.path.to_string_lossy().to_string(), mtime, size, blob)
        }
        None => (String::new(), 0i64, 0i64, None),
    };

    let r = conn.execute(
        "INSERT OR REPLACE INTO modules (module_name, path, mtime_secs, file_size, source, analysis, extract_version)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
        params![module_name, path_str, mtime, size, source, analysis_blob, EXTRACT_VERSION],
    );
    if let Err(e) = r {
        log::warn!("Failed to save module cache for '{}': {}", module_name, e);
    }
}

#[cfg(test)]
#[path = "module_cache_tests.rs"]
mod tests;
