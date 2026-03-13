//! SQLite persistence for the module cache.
//!
//! Stores resolved module exports to disk so they survive LSP restarts.
//! Validates entries against mtime + file size to detect stale data.
//! Invalidates the entire cache when `@INC` changes.

use std::collections::HashMap;
use std::path::PathBuf;
use std::time::SystemTime;

use dashmap::DashMap;
use rusqlite::{params, Connection};

use crate::file_analysis::{inferred_type_from_tag, inferred_type_to_tag};
use crate::module_index::{ExportedParam, ExportedSub, ModuleExports};

const SCHEMA_VERSION: &str = "8";

/// Bumped when extraction logic changes (new fields, better parsing).
/// Unlike SCHEMA_VERSION, this doesn't drop the table — stale entries
/// are re-resolved lazily with priority.
pub const EXTRACT_VERSION: i64 = 2;

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
            module_name  TEXT PRIMARY KEY,
            path         TEXT NOT NULL,
            mtime_secs   INTEGER NOT NULL,
            file_size    INTEGER NOT NULL,
            export       TEXT NOT NULL,
            export_ok    TEXT NOT NULL,
            source       TEXT NOT NULL DEFAULT 'import',
            subs         TEXT NOT NULL DEFAULT '{}',
            parents      TEXT NOT NULL DEFAULT '[]',
            extract_version INTEGER NOT NULL DEFAULT 0
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
                    module_name  TEXT PRIMARY KEY,
                    path         TEXT NOT NULL,
                    mtime_secs   INTEGER NOT NULL,
                    file_size    INTEGER NOT NULL,
                    export       TEXT NOT NULL,
                    export_ok    TEXT NOT NULL,
                    source       TEXT NOT NULL DEFAULT 'import',
                    subs         TEXT NOT NULL DEFAULT '{}',
                    parents      TEXT NOT NULL DEFAULT '[]',
                    extract_version INTEGER NOT NULL DEFAULT 0
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

pub fn warm_cache(
    conn: &Connection,
    cache: &DashMap<String, Option<ModuleExports>>,
) -> (usize, Vec<String>) {
    let mut stmt = match conn.prepare(
        "SELECT module_name, path, mtime_secs, file_size, export, export_ok, subs, extract_version, parents FROM modules",
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
            row.get::<_, String>(4)?,
            row.get::<_, String>(5)?,
            row.get::<_, String>(6)?,
            row.get::<_, i64>(7)?,
            row.get::<_, String>(8)?,
        ))
    }) {
        Ok(r) => r,
        Err(_) => return (0, Vec::new()),
    };

    let mut count = 0usize;
    let mut stale_names = Vec::new();
    for row in rows.flatten() {
        let (module_name, path_str, cached_mtime, cached_size, export_json, export_ok_json, subs_json, row_extract_version, parents_json) = row;

        // Negative sentinel
        if path_str.is_empty() {
            cache.insert(module_name, None);
            count += 1;
            continue;
        }

        let path = PathBuf::from(&path_str);

        // Validate mtime — skip entries where the file changed on disk
        if let Some((disk_mtime, disk_size)) = mtime_as_secs(&path) {
            if disk_mtime != cached_mtime || disk_size != cached_size {
                continue;
            }
        } else {
            continue; // file deleted
        }

        let export: Vec<String> = serde_json::from_str(&export_json).unwrap_or_default();
        let export_ok: Vec<String> = serde_json::from_str(&export_ok_json).unwrap_or_default();

        // Deserialize subs: JSON map of name → { def_line, params, is_method, return_type, hash_keys }
        let subs = deserialize_subs_json(&subs_json);
        let parents: Vec<String> = serde_json::from_str(&parents_json).unwrap_or_default();

        if export.is_empty() && export_ok.is_empty() {
            cache.insert(module_name, None);
        } else {
            // Check if this entry was produced by an older extraction version
            if row_extract_version < EXTRACT_VERSION {
                stale_names.push(module_name.clone());
            }
            cache.insert(
                module_name,
                Some(ModuleExports {
                    path,
                    export,
                    export_ok,
                    subs,
                    parents,
                }),
            );
        }
        count += 1;
    }

    (count, stale_names)
}

fn deserialize_subs_json(json_str: &str) -> HashMap<String, ExportedSub> {
    let obj: HashMap<String, serde_json::Value> = match serde_json::from_str(json_str) {
        Ok(v) => v,
        Err(_) => return HashMap::new(),
    };
    obj.into_iter()
        .filter_map(|(name, val)| {
            let def_line = val.get("def_line").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
            let is_method = val.get("is_method").and_then(|v| v.as_bool()).unwrap_or(false);
            let params = val
                .get("params")
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|p| {
                            Some(ExportedParam {
                                name: p.get("name")?.as_str()?.to_string(),
                                is_slurpy: p
                                    .get("is_slurpy")
                                    .and_then(|v| v.as_bool())
                                    .unwrap_or(false),
                            })
                        })
                        .collect()
                })
                .unwrap_or_default();
            let return_type = val
                .get("return_type")
                .and_then(|v| v.as_str())
                .and_then(inferred_type_from_tag);
            let hash_keys = val
                .get("hash_keys")
                .and_then(|v| v.as_array())
                .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
                .unwrap_or_default();
            let doc = val.get("doc")
                .and_then(|v| v.as_str())
                .map(String::from);
            Some((
                name,
                ExportedSub {
                    def_line,
                    params,
                    is_method,
                    return_type,
                    hash_keys,
                    doc,
                },
            ))
        })
        .collect()
}

pub fn save_to_db(
    conn: &Connection,
    module_name: &str,
    result: &Option<ModuleExports>,
    source: &str,
) {
    let (path_str, mtime, size, export_json, export_ok_json, subs_json, parents_json) = match result {
        Some(exports) => {
            let (mtime, size) = mtime_as_secs(&exports.path).unwrap_or((0, 0));
            let ej = serde_json::to_string(&exports.export).unwrap_or_default();
            let eoj = serde_json::to_string(&exports.export_ok).unwrap_or_default();
            let sj = serialize_subs_json(&exports.subs);
            let pj = serde_json::to_string(&exports.parents).unwrap_or_else(|_| "[]".to_string());
            (
                exports.path.to_string_lossy().to_string(),
                mtime,
                size,
                ej,
                eoj,
                sj,
                pj,
            )
        }
        None => (
            String::new(),
            0i64,
            0i64,
            "[]".to_string(),
            "[]".to_string(),
            "{}".to_string(),
            "[]".to_string(),
        ),
    };

    let r = conn.execute(
        "INSERT OR REPLACE INTO modules (module_name, path, mtime_secs, file_size, export, export_ok, source, subs, parents, extract_version)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)",
        params![module_name, path_str, mtime, size, export_json, export_ok_json, source, subs_json, parents_json, EXTRACT_VERSION],
    );
    if let Err(e) = r {
        log::warn!("Failed to save module cache for '{}': {}", module_name, e);
    }
}

fn serialize_subs_json(subs: &HashMap<String, ExportedSub>) -> String {
    let mut map = serde_json::Map::new();
    for (name, sub_info) in subs {
        let mut obj = serde_json::Map::new();
        obj.insert("def_line".into(), sub_info.def_line.into());
        obj.insert("is_method".into(), sub_info.is_method.into());
        let params: Vec<serde_json::Value> = sub_info
            .params
            .iter()
            .map(|p| {
                serde_json::json!({
                    "name": p.name,
                    "is_slurpy": p.is_slurpy,
                })
            })
            .collect();
        obj.insert("params".into(), params.into());
        if let Some(ref rt) = sub_info.return_type {
            obj.insert(
                "return_type".into(),
                serde_json::Value::String(inferred_type_to_tag(rt)),
            );
        }
        if !sub_info.hash_keys.is_empty() {
            obj.insert("hash_keys".into(), serde_json::json!(sub_info.hash_keys));
        }
        if let Some(ref doc) = sub_info.doc {
            obj.insert("doc".into(), serde_json::Value::String(doc.clone()));
        }
        map.insert(name.clone(), obj.into());
    }
    serde_json::Value::Object(map).to_string()
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
            subs: HashMap::new(),
            parents: vec![],
        });
        save_to_db(&conn, "TestModule", &exports, "import");

        let cache = DashMap::new();
        let (n, stale) = warm_cache(&conn, &cache);
        assert_eq!(n, 1);
        assert!(stale.is_empty());

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
        let (n, _) = warm_cache(&conn, &cache);
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
            subs: HashMap::new(),
            parents: vec![],
        });
        save_to_db(&conn, "StaleModule", &exports, "import");

        std::thread::sleep(std::time::Duration::from_secs(1));
        std::fs::write(&pm, "v2 with more content").unwrap();

        let cache = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
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
        let cache = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
        assert_eq!(n, 0, "old data should be gone after migration");
    }

    #[test]
    fn test_db_migration_old_to_v5_supports_subs() {
        // Simulate an old database (different schema version)
        let conn = Connection::open_in_memory().unwrap();
        conn.execute_batch(
            "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT NOT NULL);
             INSERT INTO meta (key, value) VALUES ('schema_version', '4');
             CREATE TABLE modules (
                 module_name TEXT PRIMARY KEY,
                 path        TEXT NOT NULL,
                 mtime_secs  INTEGER NOT NULL,
                 file_size   INTEGER NOT NULL,
                 export      TEXT NOT NULL,
                 export_ok   TEXT NOT NULL,
                 source      TEXT NOT NULL DEFAULT 'import',
                 return_types TEXT NOT NULL DEFAULT '{}',
                 hash_keys    TEXT NOT NULL DEFAULT '{}'
             );",
        )
        .unwrap();

        // Run migration
        init_schema(&conn).unwrap();

        // Now save with subs — should not fail
        let dir = std::env::temp_dir();
        let pm = dir.join("MigratedModule.pm");
        std::fs::write(&pm, "package MigratedModule; 1;").unwrap();

        let mut subs = HashMap::new();
        subs.insert(
            "get_data".to_string(),
            ExportedSub {
                def_line: 10,
                params: vec![],
                is_method: false,
                return_type: Some(crate::file_analysis::InferredType::HashRef),
                hash_keys: vec!["host".into()],
                doc: None,
            },
        );

        let exports = Some(ModuleExports {
            path: pm.clone(),
            export: vec!["get_data".into()],
            export_ok: vec![],
            subs,
            parents: vec![],
        });
        save_to_db(&conn, "MigratedModule", &exports, "import");

        // Verify it round-trips
        let cache = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
        assert_eq!(n, 1);
        let loaded = cache.get("MigratedModule").unwrap();
        let loaded = loaded.as_ref().unwrap();
        let sub_info = loaded.subs.get("get_data").expect("should have get_data sub");
        assert_eq!(sub_info.def_line, 10);
        assert_eq!(
            sub_info.return_type,
            Some(crate::file_analysis::InferredType::HashRef),
            "return_type should survive migration + roundtrip"
        );
        assert_eq!(sub_info.hash_keys, vec!["host"]);

        let _ = std::fs::remove_file(&pm);
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
            subs: HashMap::new(),
            parents: vec![],
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

    #[test]
    fn test_db_subs_roundtrip() {
        use crate::file_analysis::InferredType;

        let conn = test_db();

        let dir = std::env::temp_dir();
        let pm = dir.join("SubsTest.pm");
        std::fs::write(&pm, "package SubsTest; 1;").unwrap();

        let mut subs = HashMap::new();
        subs.insert(
            "get_config".to_string(),
            ExportedSub {
                def_line: 5,
                params: vec![
                    ExportedParam { name: "$path".into(), is_slurpy: false },
                ],
                is_method: false,
                return_type: Some(InferredType::HashRef),
                hash_keys: vec!["host".into(), "port".into()],
                doc: None,
            },
        );
        subs.insert(
            "make_items".to_string(),
            ExportedSub {
                def_line: 20,
                params: vec![],
                is_method: false,
                return_type: Some(InferredType::ArrayRef),
                hash_keys: vec![],
                doc: None,
            },
        );
        subs.insert(
            "new_obj".to_string(),
            ExportedSub {
                def_line: 30,
                params: vec![ExportedParam { name: "$class".into(), is_slurpy: false }],
                is_method: true,
                return_type: Some(InferredType::ClassName("MyObj".into())),
                hash_keys: vec![],
                doc: None,
            },
        );

        let exports = Some(ModuleExports {
            path: pm.clone(),
            export: vec!["get_config".into()],
            export_ok: vec!["make_items".into(), "new_obj".into()],
            subs,
            parents: vec![],
        });
        save_to_db(&conn, "SubsTest", &exports, "import");

        let cache = DashMap::new();
        let (n, _) = warm_cache(&conn, &cache);
        assert_eq!(n, 1);

        let loaded = cache.get("SubsTest").unwrap();
        let loaded = loaded.as_ref().unwrap();

        let gc = loaded.subs.get("get_config").expect("get_config");
        assert_eq!(gc.def_line, 5);
        assert_eq!(gc.params.len(), 1);
        assert_eq!(gc.params[0].name, "$path");
        assert_eq!(gc.return_type, Some(InferredType::HashRef));
        assert_eq!(gc.hash_keys, vec!["host", "port"]);

        let mi = loaded.subs.get("make_items").expect("make_items");
        assert_eq!(mi.def_line, 20);
        assert_eq!(mi.return_type, Some(InferredType::ArrayRef));

        let no = loaded.subs.get("new_obj").expect("new_obj");
        assert_eq!(no.def_line, 30);
        assert!(no.is_method);
        assert_eq!(no.return_type, Some(InferredType::ClassName("MyObj".into())));

        let _ = std::fs::remove_file(&pm);
    }

    #[test]
    fn test_warm_reports_stale_entries() {
        let conn = test_db();
        let dir = std::env::temp_dir();
        let pm = dir.join("StaleExtract.pm");
        std::fs::write(&pm, "package StaleExtract; 1;").unwrap();

        let (mtime, size) = mtime_as_secs(&pm).unwrap();

        // Save with old extract version (0, below EXTRACT_VERSION)
        conn.execute(
            "INSERT INTO modules (module_name, path, mtime_secs, file_size, export, export_ok, subs, extract_version)
             VALUES ('StaleExtract', ?1, ?2, ?3, '[\"foo\"]', '[]', '{}', 0)",
            params![pm.to_string_lossy(), mtime, size],
        ).unwrap();

        let cache = DashMap::new();
        let (loaded, stale) = warm_cache(&conn, &cache);
        assert_eq!(loaded, 1, "stale entry should still be loaded");
        assert!(cache.contains_key("StaleExtract"), "stale entry should be in cache");
        assert_eq!(stale, vec!["StaleExtract"], "should report as stale");

        let _ = std::fs::remove_file(&pm);
    }

    #[test]
    fn test_warm_fresh_not_stale() {
        let conn = test_db();
        let dir = std::env::temp_dir();
        let pm = dir.join("FreshExtract.pm");
        std::fs::write(&pm, "package FreshExtract; 1;").unwrap();

        let exports = Some(ModuleExports {
            path: pm.clone(),
            export: vec!["foo".into()],
            export_ok: vec![],
            subs: HashMap::new(),
            parents: vec![],
        });
        save_to_db(&conn, "FreshExtract", &exports, "import");

        let cache = DashMap::new();
        let (loaded, stale) = warm_cache(&conn, &cache);
        assert_eq!(loaded, 1);
        assert!(stale.is_empty(), "current-version entry should not be stale");

        let _ = std::fs::remove_file(&pm);
    }

    #[test]
    fn test_save_writes_extract_version() {
        let conn = test_db();
        let dir = std::env::temp_dir();
        let pm = dir.join("VersionedSave.pm");
        std::fs::write(&pm, "package VersionedSave; 1;").unwrap();

        let exports = Some(ModuleExports {
            path: pm.clone(),
            export: vec!["foo".into()],
            export_ok: vec![],
            subs: HashMap::new(),
            parents: vec![],
        });
        save_to_db(&conn, "VersionedSave", &exports, "import");

        let ver: i64 = conn.query_row(
            "SELECT extract_version FROM modules WHERE module_name = 'VersionedSave'",
            [], |row| row.get(0),
        ).unwrap();
        assert_eq!(ver, EXTRACT_VERSION);

        let _ = std::fs::remove_file(&pm);
    }
}
