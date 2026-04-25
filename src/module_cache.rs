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
pub const EXTRACT_VERSION: i64 = 10;

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
mod tests {
    use super::*;
    use rusqlite::Connection;

    fn test_db() -> Connection {
        let conn = Connection::open_in_memory().unwrap();
        init_schema(&conn).unwrap();
        conn
    }

    fn parse_source_to_cached(source: &str, path: &std::path::Path) -> Arc<CachedModule> {
        use tree_sitter::Parser;
        let mut parser = Parser::new();
        parser.set_language(&ts_parser_perl::LANGUAGE.into()).unwrap();
        let tree = parser.parse(source, None).unwrap();
        let fa = crate::builder::build(&tree, source.as_bytes());
        Arc::new(CachedModule::new(path.to_path_buf(), Arc::new(fa)))
    }

    #[test]
    fn test_db_save_and_load_roundtrip() {
        let conn = test_db();
        let dir = std::env::temp_dir();
        let pm = dir.join("TestModule_roundtrip.pm");
        std::fs::write(&pm, "package TestModule;\nour @EXPORT = qw(foo bar);\nour @EXPORT_OK = qw(baz);\nsub foo { 1 }\nsub bar { 2 }\nsub baz { 3 }\n1;\n").unwrap();

        let source = std::fs::read_to_string(&pm).unwrap();
        let cached = Some(parse_source_to_cached(&source, &pm));
        save_to_db(&conn, "TestModule", &cached, "import");

        let cache: DashMap<String, Option<Arc<CachedModule>>> = DashMap::new();
        let (n, stale) = warm_cache(&conn, &cache);
        assert_eq!(n, 1);
        assert!(stale.is_empty());

        let loaded = cache.get("TestModule").unwrap();
        let loaded = loaded.as_ref().unwrap();
        assert_eq!(loaded.analysis.export, vec!["foo", "bar"]);
        assert_eq!(loaded.analysis.export_ok, vec!["baz"]);

        let _ = std::fs::remove_file(&pm);
    }

    /// Pin-the-fix: `plugin_namespaces` survives the bincode +
    /// zstd + SQLite round trip with entities, bridges, and
    /// plugin_id intact. Without this test, schema drift on the
    /// PluginNamespace struct would silently truncate cached
    /// modules and we'd notice only when cross-file bridge lookups
    /// mysteriously missed entries.
    #[test]
    fn test_db_plugin_namespaces_roundtrip() {
        let conn = test_db();
        let dir = std::env::temp_dir();
        let pm = dir.join("TestMojoApp_namespaces.pm");
        // A Mojolicious::Lite script — mojo-lite + mojo-routes +
        // mojo-helpers should all emit namespaces that round-trip.
        std::fs::write(&pm,
            "package TestMojoApp;\n\
             use Mojolicious::Lite;\n\
             app->helper(current_user => sub { my ($c) = @_; });\n\
             get '/users' => sub { my $c = shift; };\n\
             1;\n"
        ).unwrap();

        let source = std::fs::read_to_string(&pm).unwrap();
        let cached = Some(parse_source_to_cached(&source, &pm));
        let original_ns_count = cached.as_ref().unwrap()
            .analysis.plugin_namespaces.len();
        assert!(original_ns_count > 0,
            "sanity: fixture must produce at least one PluginNamespace");

        save_to_db(&conn, "TestMojoApp", &cached, "import");

        let cache: DashMap<String, Option<Arc<CachedModule>>> = DashMap::new();
        let (n, stale) = warm_cache(&conn, &cache);
        assert_eq!(n, 1);
        assert!(stale.is_empty(), "fresh insert should not be stale");

        let loaded = cache.get("TestMojoApp").unwrap();
        let loaded = loaded.as_ref().unwrap();
        let loaded_ns = &loaded.analysis.plugin_namespaces;
        assert_eq!(loaded_ns.len(), original_ns_count,
            "PluginNamespace count must round-trip; got: {:?}", loaded_ns);

        // Every namespace must preserve its plugin_id, kind, and at
        // least one Bridge::Class — the three fields that `bridges_index`
        // and `for_each_entity_bridged_to` depend on.
        for ns in loaded_ns {
            assert!(!ns.plugin_id.is_empty(), "plugin_id preserved");
            assert!(!ns.kind.is_empty(), "kind preserved");
            assert!(!ns.bridges.is_empty(), "bridges preserved");
            assert!(ns.bridges.iter().any(|b|
                matches!(b, crate::file_analysis::Bridge::Class(_))),
                "at least one Class bridge survives");
        }

        let _ = std::fs::remove_file(&pm);
    }

    #[test]
    fn test_db_negative_result_roundtrip() {
        let conn = test_db();
        save_to_db(&conn, "Nonexistent::Module", &None, "import");

        let cache: DashMap<String, Option<Arc<CachedModule>>> = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
        assert_eq!(n, 1);

        let entry = cache.get("Nonexistent::Module").unwrap();
        assert!(entry.is_none());
    }

    #[test]
    fn test_db_stale_entry_skipped() {
        let conn = test_db();

        let dir = std::env::temp_dir();
        let pm = dir.join("StaleModule_v9.pm");
        std::fs::write(&pm, "package StaleModule;\nour @EXPORT_OK = qw(old);\nsub old {}\n1;\n").unwrap();

        let source = std::fs::read_to_string(&pm).unwrap();
        let cached = Some(parse_source_to_cached(&source, &pm));
        save_to_db(&conn, "StaleModule", &cached, "import");

        std::thread::sleep(std::time::Duration::from_secs(1));
        std::fs::write(&pm, "package StaleModule;\nour @EXPORT_OK = qw(v2 with more content);\n1;\n").unwrap();

        let cache: DashMap<String, Option<Arc<CachedModule>>> = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
        assert_eq!(n, 0, "stale entry should not be loaded");
        assert!(!cache.contains_key("StaleModule"));

        let _ = std::fs::remove_file(&pm);
    }

    #[test]
    fn test_db_plugin_fingerprint_invalidation() {
        let conn = test_db();

        // First run: claims plugin set fingerprint "hash-A".
        validate_plugin_fingerprint(&conn, "hash-A").unwrap();
        save_to_db(&conn, "Foo", &None, "import");

        // Same fingerprint → cache survives.
        validate_plugin_fingerprint(&conn, "hash-A").unwrap();
        let cache: DashMap<String, Option<Arc<CachedModule>>> = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
        assert_eq!(n, 1, "cache should survive identical fingerprint");

        // Plugin set changed → cache cleared.
        validate_plugin_fingerprint(&conn, "hash-B").unwrap();
        let cache: DashMap<String, Option<Arc<CachedModule>>> = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
        assert_eq!(n, 0, "cache should be empty after plugin set change");

        // Stamp persists — second run with hash-B doesn't re-clear.
        save_to_db(&conn, "Bar", &None, "import");
        validate_plugin_fingerprint(&conn, "hash-B").unwrap();
        let cache: DashMap<String, Option<Arc<CachedModule>>> = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
        assert_eq!(n, 1, "stamp should persist between same-fingerprint runs");
    }

    #[test]
    fn test_db_inc_hash_invalidation() {
        let conn = test_db();
        let paths1 = vec![PathBuf::from("/usr/lib/perl5")];
        let paths2 = vec![
            PathBuf::from("/usr/lib/perl5"),
            PathBuf::from("/home/user/lib"),
        ];

        validate_inc_paths(&conn, &paths1).unwrap();
        save_to_db(&conn, "Foo", &None, "import");

        validate_inc_paths(&conn, &paths2).unwrap();
        let cache: DashMap<String, Option<Arc<CachedModule>>> = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
        assert_eq!(n, 0, "cache should be empty after @INC change");
    }

    #[test]
    fn test_db_schema_version_migration() {
        let conn = test_db();

        conn.execute(
            "UPDATE meta SET value = '0' WHERE key = 'schema_version'",
            [],
        )
        .unwrap();
        save_to_db(&conn, "OldModule", &None, "import");

        init_schema(&conn).unwrap();
        let cache: DashMap<String, Option<Arc<CachedModule>>> = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
        assert_eq!(n, 0, "old data should be gone after migration");
    }

    #[test]
    fn test_db_source_column() {
        let conn = test_db();
        let dir = std::env::temp_dir();
        let pm = dir.join("SourceTest_v9.pm");
        std::fs::write(&pm, "package SourceTest;\nour @EXPORT_OK = qw(foo);\nsub foo {}\n1;\n").unwrap();

        let source = std::fs::read_to_string(&pm).unwrap();
        let cached = Some(parse_source_to_cached(&source, &pm));
        save_to_db(&conn, "SourceTest", &cached, "cpanfile");

        let source_val: String = conn
            .query_row(
                "SELECT source FROM modules WHERE module_name = 'SourceTest'",
                [],
                |row| row.get(0),
            )
            .unwrap();
        assert_eq!(source_val, "cpanfile");

        let _ = std::fs::remove_file(&pm);
    }

    #[test]
    fn test_workspace_cache_dir_uniqueness() {
        let d1 = cache_dir_for_workspace(Some("file:///home/user/project-a"));
        let d2 = cache_dir_for_workspace(Some("file:///home/user/project-b"));
        let d_none = cache_dir_for_workspace(None);
        assert_ne!(d1, d2, "Different roots should produce different paths");
        assert_ne!(d1, d_none, "Root vs no-root should differ");
        assert_eq!(
            d1,
            cache_dir_for_workspace(Some("file:///home/user/project-a")),
            "Same root should produce same path"
        );
    }

    #[test]
    fn test_full_file_analysis_survives_roundtrip() {
        // Verify that FileAnalysis fields lost in the old ModuleExports representation
        // (refs, type_constraints, call_bindings, full package_parents) now survive.
        let conn = test_db();
        let dir = std::env::temp_dir();
        let pm = dir.join("Fidelity_v9.pm");
        std::fs::write(
            &pm,
            "package Fidelity;\nuse parent 'Base';\nour @EXPORT_OK = qw(make);\nsub make { return { host => 1, port => 2 } }\n1;\n",
        )
        .unwrap();

        let source = std::fs::read_to_string(&pm).unwrap();
        let cached = parse_source_to_cached(&source, &pm);
        let original_refs_count = cached.analysis.refs.len();
        let original_package_parents = cached.analysis.package_parents.clone();
        save_to_db(&conn, "Fidelity", &Some(Arc::clone(&cached)), "import");

        let cache: DashMap<String, Option<Arc<CachedModule>>> = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
        assert_eq!(n, 1);

        let loaded = cache.get("Fidelity").unwrap();
        let loaded = loaded.as_ref().unwrap();
        assert_eq!(loaded.analysis.refs.len(), original_refs_count, "refs survive roundtrip");
        assert_eq!(loaded.analysis.package_parents, original_package_parents, "package_parents survive");

        let _ = std::fs::remove_file(&pm);
    }
}
