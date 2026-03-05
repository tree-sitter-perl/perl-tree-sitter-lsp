//! SQLite persistence for the module cache.
//!
//! Stores resolved module exports to disk so they survive LSP restarts.
//! Validates entries against mtime + file size to detect stale data.
//! Invalidates the entire cache when `@INC` changes.

use std::path::PathBuf;
use std::time::SystemTime;

use dashmap::DashMap;
use rusqlite::{params, Connection};

use crate::module_index::ModuleExports;

const SCHEMA_VERSION: &str = "2";

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
            module_name TEXT PRIMARY KEY,
            path        TEXT NOT NULL,
            mtime_secs  INTEGER NOT NULL,
            file_size   INTEGER NOT NULL,
            export      TEXT NOT NULL,
            export_ok   TEXT NOT NULL,
            source      TEXT NOT NULL DEFAULT 'import'
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
                    module_name TEXT PRIMARY KEY,
                    path        TEXT NOT NULL,
                    mtime_secs  INTEGER NOT NULL,
                    file_size   INTEGER NOT NULL,
                    export      TEXT NOT NULL,
                    export_ok   TEXT NOT NULL,
                    source      TEXT NOT NULL DEFAULT 'import'
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

fn mtime_as_secs(path: &std::path::Path) -> Option<(i64, i64)> {
    let meta = std::fs::metadata(path).ok()?;
    let mtime = meta.modified().ok()?;
    let secs = mtime.duration_since(SystemTime::UNIX_EPOCH).ok()?.as_secs() as i64;
    let size = meta.len() as i64;
    Some((secs, size))
}

pub fn warm_cache(conn: &Connection, cache: &DashMap<String, Option<ModuleExports>>) -> usize {
    let mut stmt = match conn.prepare(
        "SELECT module_name, path, mtime_secs, file_size, export, export_ok FROM modules",
    ) {
        Ok(s) => s,
        Err(_) => return 0,
    };

    let rows = match stmt.query_map([], |row| {
        Ok((
            row.get::<_, String>(0)?,
            row.get::<_, String>(1)?,
            row.get::<_, i64>(2)?,
            row.get::<_, i64>(3)?,
            row.get::<_, String>(4)?,
            row.get::<_, String>(5)?,
        ))
    }) {
        Ok(r) => r,
        Err(_) => return 0,
    };

    let mut count = 0usize;
    for row in rows.flatten() {
        let (module_name, path_str, cached_mtime, cached_size, export_json, export_ok_json) = row;

        // Negative sentinel
        if path_str.is_empty() {
            cache.insert(module_name, None);
            count += 1;
            continue;
        }

        let path = PathBuf::from(&path_str);

        // Validate mtime — skip stale entries
        if let Some((disk_mtime, disk_size)) = mtime_as_secs(&path) {
            if disk_mtime != cached_mtime || disk_size != cached_size {
                continue;
            }
        } else {
            continue; // file deleted
        }

        let export: Vec<String> = serde_json::from_str(&export_json).unwrap_or_default();
        let export_ok: Vec<String> = serde_json::from_str(&export_ok_json).unwrap_or_default();

        if export.is_empty() && export_ok.is_empty() {
            cache.insert(module_name, None);
        } else {
            cache.insert(
                module_name,
                Some(ModuleExports {
                    path,
                    export,
                    export_ok,
                }),
            );
        }
        count += 1;
    }

    count
}

pub fn save_to_db(
    conn: &Connection,
    module_name: &str,
    result: &Option<ModuleExports>,
    source: &str,
) {
    let (path_str, mtime, size, export_json, export_ok_json) = match result {
        Some(exports) => {
            let (mtime, size) = mtime_as_secs(&exports.path).unwrap_or((0, 0));
            let ej = serde_json::to_string(&exports.export).unwrap_or_default();
            let eoj = serde_json::to_string(&exports.export_ok).unwrap_or_default();
            (
                exports.path.to_string_lossy().to_string(),
                mtime,
                size,
                ej,
                eoj,
            )
        }
        None => (
            String::new(),
            0i64,
            0i64,
            "[]".to_string(),
            "[]".to_string(),
        ),
    };

    let r = conn.execute(
        "INSERT OR REPLACE INTO modules (module_name, path, mtime_secs, file_size, export, export_ok, source)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7)",
        params![module_name, path_str, mtime, size, export_json, export_ok_json, source],
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

    #[test]
    fn test_db_save_and_load_roundtrip() {
        let conn = test_db();

        let dir = std::env::temp_dir();
        let pm = dir.join("TestModule.pm");
        std::fs::write(&pm, "package TestModule; 1;").unwrap();

        let exports = Some(ModuleExports {
            path: pm.clone(),
            export: vec!["foo".into(), "bar".into()],
            export_ok: vec!["baz".into()],
        });
        save_to_db(&conn, "TestModule", &exports, "import");

        let cache = DashMap::new();
        let n = warm_cache(&conn, &cache);
        assert_eq!(n, 1);

        let loaded = cache.get("TestModule").unwrap();
        let loaded = loaded.as_ref().unwrap();
        assert_eq!(loaded.export, vec!["foo", "bar"]);
        assert_eq!(loaded.export_ok, vec!["baz"]);
        assert_eq!(loaded.path, pm);

        let _ = std::fs::remove_file(&pm);
    }

    #[test]
    fn test_db_negative_result_roundtrip() {
        let conn = test_db();
        save_to_db(&conn, "Nonexistent::Module", &None, "import");

        let cache = DashMap::new();
        let n = warm_cache(&conn, &cache);
        assert_eq!(n, 1);

        let entry = cache.get("Nonexistent::Module").unwrap();
        assert!(entry.is_none());
    }

    #[test]
    fn test_db_stale_entry_skipped() {
        let conn = test_db();

        let dir = std::env::temp_dir();
        let pm = dir.join("StaleModule.pm");
        std::fs::write(&pm, "v1").unwrap();

        let exports = Some(ModuleExports {
            path: pm.clone(),
            export: vec!["old".into()],
            export_ok: vec![],
        });
        save_to_db(&conn, "StaleModule", &exports, "import");

        std::thread::sleep(std::time::Duration::from_secs(1));
        std::fs::write(&pm, "v2 with more content").unwrap();

        let cache = DashMap::new();
        let n = warm_cache(&conn, &cache);
        assert_eq!(n, 0, "stale entry should not be loaded");
        assert!(!cache.contains_key("StaleModule"));

        let _ = std::fs::remove_file(&pm);
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
        let cache = DashMap::new();
        let n = warm_cache(&conn, &cache);
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
        let cache = DashMap::new();
        let n = warm_cache(&conn, &cache);
        assert_eq!(n, 0, "old data should be gone after migration");
    }

    #[test]
    fn test_db_source_column() {
        let conn = test_db();
        let dir = std::env::temp_dir();
        let pm = dir.join("SourceTest.pm");
        std::fs::write(&pm, "package SourceTest; 1;").unwrap();

        let exports = Some(ModuleExports {
            path: pm.clone(),
            export: vec!["foo".into()],
            export_ok: vec![],
        });
        save_to_db(&conn, "SourceTest", &exports, "cpanfile");

        let source: String = conn
            .query_row(
                "SELECT source FROM modules WHERE module_name = 'SourceTest'",
                [],
                |row| row.get(0),
            )
            .unwrap();
        assert_eq!(source, "cpanfile");

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
}
