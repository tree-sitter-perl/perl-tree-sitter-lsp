# perl-lsp for Claude Code

Gives Claude Code real-time Perl intelligence — type inference, cross-file
navigation, and framework awareness — by wiring up the
[perl-lsp](https://github.com/tree-sitter-perl/perl-tree-sitter-lsp) language
server over LSP.

## Prerequisite: install the language server

This plugin is **configuration only**. It tells Claude Code how to talk to
`perl-lsp`; it does **not** download or run anything on its own. Install the
binary yourself first so it's on your `PATH`. Either download a prebuilt binary
for your platform (macOS, Linux, Windows) from the
[releases page](https://github.com/tree-sitter-perl/perl-tree-sitter-lsp/releases)
and put `perl-lsp` on your `PATH`, or build from source with a Rust toolchain:

```bash
cargo install perl-lsp
```

Verify it's reachable:

```bash
perl-lsp --version
```

If the binary is missing, Claude Code reports `Executable not found in $PATH`
in the `/plugin` **Errors** tab and Perl features stay off until you install it.

## Install the plugin

`perl-lsp` is distributed through the public Claude Code plugin marketplaces.
Once you've added one of them, install it by name:

```
/plugin install perl-lsp@<marketplace>
```

The server starts lazily — the first time you open a `.pl`, `.pm`, or `.t`
file in a session — and speaks stdio. No flags, no configuration.

### Try it before it's published (local)

The plugin needs no marketplace to run locally. Copy this directory into your
skills directory and Claude Code auto-loads it on the next session:

```bash
cp -r editors/claude-code ~/.claude/skills/perl-lsp
```

It loads as `perl-lsp@skills-dir` with no install step. (Still requires the
`perl-lsp` binary on `PATH`.)

## What you get

- **Type inference, no annotations** — types are inferred from how values are
  used, and flow across files and through method chains.
- **Navigation** — go-to-definition and find-references, scope-aware and
  cross-file (including through inheritance).
- **Framework awareness** — Moo/Moose `has`, Mojolicious, and DBIx::Class
  contribute real symbols, so completion and goto-def understand them.
- **Diagnostics** — unresolved function/method hints, deliberately
  low-severity (dynamic Perl is normal). Set `"diagnostics": false` in
  `.lsp.json` if you want navigation without diagnostic injection.

## Trust note

The only Perl that ever runs is `perl-lsp`'s short `@INC` probe at startup;
everything else is static analysis over the parse tree. This plugin adds no
hooks and downloads nothing — you stay in control of what lands on your
machine.
