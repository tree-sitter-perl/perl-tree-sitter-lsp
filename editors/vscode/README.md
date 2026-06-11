# Perl (perl-lsp)

Perl language support powered by [tree-sitter-perl](https://github.com/tree-sitter-perl/tree-sitter-perl) — type inference without annotations, cross-file navigation, and framework intelligence for Moo/Moose, Mojolicious, and DBIx::Class.

The extension manages the [perl-lsp](https://github.com/tree-sitter-perl/perl-tree-sitter-lsp) language server for you: it downloads the matching binary on first activation, so installing the extension is the only setup step.

## Highlights

### Type inference, no annotations

perl-lsp infers types from how your code uses values:

```perl
my $ua = Mojo::UserAgent->new;        # $ua is Mojo::UserAgent
my $res = $ua->get('/api')->result;   # chains resolve through return types
$res->json->{items};                  # hash-key completion follows
```

- `Foo->new()`, `bless`, `ref` checks, and literals all feed inference
- Method chains propagate types — `$self->get_config()->{host}` resolves through return types
- Return types and parameter types flow across module boundaries

### Framework intelligence

| Framework | What perl-lsp understands |
|-----------|--------------------------|
| **Moo/Moose** | `has` accessor synthesis with `is`/`isa` type mapping, `extends`/`with` for inheritance and roles |
| **Mojo::Base** | Accessor synthesis, fluent setter chaining, parent class detection |
| **DBIx::Class** | `add_columns` accessors, `has_many`/`belongs_to`/`has_one` relationships, `load_components` mixins |
| **Perl 5.38 `class`** | `field` with `:param`/`:reader`/`:writer`, `:isa(Parent)`, `:does(Role)`, implicit `$self` |

DSL keywords (`has`, `with`, `extends`, `around`, …) are recognized and won't trigger unresolved-function diagnostics.

### Dynamic dispatch that actually resolves

```perl
my $method = "get_$field";
$self->$method();          # go-to-definition works — resolves through the constant
```

perl-lsp folds `use constant`, package-scope variables, string interpolation, and loop variables to resolve dynamic method calls.

### Cross-file intelligence

- Module resolution from `@INC`, `PERL5LIB`, and cpanfile dependencies
- Auto-discovers `lib/` and `local/lib/perl5/` — no configuration for standard layouts (cpm, carton, plain `lib/`)
- Inheritance chain walking: parents, roles, mixins, `load_components`
- Cross-file rename — e.g. 289 edits across 56 files in Mojolicious in under a second
- SQLite cache per project for instant warm starts

## What you get

completion (variables, methods, hash keys, auto-import, module names) · go-to-definition · find references · rename · hover with rendered POD · signature help · semantic tokens · inlay hints · diagnostics · code actions · workspace symbols · document outline · formatting (perltidy) · document highlights · selection range · folding · linked editing

## Requirements

None for supported platforms — the extension downloads a prebuilt server binary from GitHub Releases on first activation and caches it.

Prebuilt binaries cover **Linux x64, macOS (Intel and Apple Silicon), and Windows x64**. On other platforms, install the server yourself (`cargo install perl-lsp`) and the extension will find it on `PATH`, or point `perl-lsp.path` at the binary.

Formatting requires `perltidy` on your `PATH` (optional).

## Settings

| Setting | Default | Description |
|---------|---------|-------------|
| `perl-lsp.path` | `""` | Path to a `perl-lsp` binary. Leave empty to use the cached/downloaded one. |

The server binary is resolved in this order: `perl-lsp.path` → previously downloaded copy → `perl-lsp` on `PATH` → download from GitHub Releases.

## Non-standard library layouts

perl-lsp finds modules in `lib/`, `local/lib/perl5/`, and your `@INC` automatically. For anything else, set `PERL5LIB` in the environment VS Code is launched from:

```bash
PERL5LIB=./my-libs/perl5 code .
```

## Extending: plugins for your in-house frameworks

Framework smarts ship as [Rhai](https://rhai.rs) plugins, and you can add your own — drop a `.rhai` file into the directory named by `$PERL_LSP_PLUGIN_DIR`. If your codebase uses an `Import::Base`-style kit, the bundled generator can emit a plugin for it automatically. See the [plugin docs](https://github.com/tree-sitter-perl/perl-tree-sitter-lsp#plugins).

## Troubleshooting

- Server logs are in the **Output** panel → **perl-lsp** channel.
- The first open of a large workspace indexes it in the background; cross-file results sharpen as indexing completes and are cached for next time.
- File an issue: [tree-sitter-perl/perl-tree-sitter-lsp](https://github.com/tree-sitter-perl/perl-tree-sitter-lsp/issues)

## License

[Artistic License 2.0](https://github.com/tree-sitter-perl/perl-tree-sitter-lsp/blob/main/LICENSE) — same as Perl itself.
