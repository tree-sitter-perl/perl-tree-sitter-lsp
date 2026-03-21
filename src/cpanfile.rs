//! cpanfile parsing: extract `requires 'Module::Name'` calls using tree-sitter queries.

use tree_sitter::Parser;

/// Parse a cpanfile at `{root}/cpanfile` and extract module names.
pub fn parse_cpanfile(root_path: &std::path::Path) -> Vec<String> {
    let cpanfile = root_path.join("cpanfile");
    let source = match std::fs::read_to_string(&cpanfile) {
        Ok(s) => s,
        Err(_) => return vec![],
    };

    let mut parser = Parser::new();
    parser
        .set_language(&ts_parser_perl::LANGUAGE.into())
        .expect("failed to set Perl language");
    let tree = match parser.parse(&source, None) {
        Some(t) => t,
        None => return vec![],
    };

    let mut modules = Vec::new();
    collect_requires(tree.root_node(), source.as_bytes(), &mut modules);
    modules.sort();
    modules.dedup();
    log::info!("cpanfile: found {} requires: {:?}", modules.len(), modules);
    modules
}

/// Use a pre-compiled tree-sitter query to find all `requires 'Module::Name'` calls.
fn collect_requires(root: tree_sitter::Node, source: &[u8], modules: &mut Vec<String>) {
    use tree_sitter::{QueryCursor, StreamingIterator};

    let query = crate::query_cache::cpanfile_requires();
    let module_idx = match query.capture_index_for_name("module") {
        Some(idx) => idx,
        None => return,
    };

    let mut cursor = QueryCursor::new();
    let mut matches = cursor.matches(query, root, source);
    while let Some(m) = matches.next() {
        for cap in m.captures {
            if cap.index == module_idx {
                if let Ok(name) = cap.node.utf8_text(source) {
                    modules.push(name.to_string());
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_cpanfile_basic() {
        let dir = std::env::temp_dir().join("perl_lsp_test_cpanfile");
        let _ = std::fs::create_dir_all(&dir);
        let cpanfile = dir.join("cpanfile");
        std::fs::write(
            &cpanfile,
            r#"requires 'Mojolicious', '>= 9.0';
requires 'DBI';
requires 'JSON::XS';

on test => sub {
    requires 'Test::More';
    requires 'Test::Deep';
};
"#,
        )
        .unwrap();

        let modules = parse_cpanfile(&dir);
        assert!(modules.contains(&"Mojolicious".to_string()), "got: {:?}", modules);
        assert!(modules.contains(&"DBI".to_string()), "got: {:?}", modules);
        assert!(modules.contains(&"JSON::XS".to_string()), "got: {:?}", modules);
        assert!(modules.contains(&"Test::More".to_string()), "got: {:?}", modules);
        assert!(modules.contains(&"Test::Deep".to_string()), "got: {:?}", modules);
        assert_eq!(modules.len(), 5);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_parse_cpanfile_missing() {
        let dir = std::env::temp_dir().join("perl_lsp_test_no_cpanfile");
        let _ = std::fs::create_dir_all(&dir);
        let _ = std::fs::remove_file(dir.join("cpanfile")); // ensure it doesn't exist

        let modules = parse_cpanfile(&dir);
        assert!(modules.is_empty());

        let _ = std::fs::remove_dir_all(&dir);
    }
}
