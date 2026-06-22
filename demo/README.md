# perl-lsp screencast

A short, scripted demo of perl-lsp's editor features, rendered headlessly.

## Artifacts

- `perl-lsp.mp4` / `perl-lsp.webm` / `perl-lsp.gif` — the demo (~23s)
- `perl-lsp.cast` — the raw [asciinema](https://asciinema.org) recording

## How it's made

We record the terminal **byte stream** (not a screenshot of a rendered screen),
so the LSP popup menu — which is just escape sequences in that stream — is
captured deterministically. Screenshot-based recorders (vhs, kitty + x11grab)
lost a GPU/capture race against the popup's brief redraw; asciinema doesn't.

```
asciinema_drive.sh   nvim in a sized tmux pane, driven by `tmux send-keys`,
                     recorded with `asciinema rec`
demo_init.lua        nvim config: the real perl-lsp setup (test_nvim_init.lua)
                     + captions + determinism helpers
agg                  renders the .cast -> gif; ffmpeg -> mp4/webm
```

## Regenerate

Needs `tmux`, `asciinema`, `agg`, `ffmpeg` on PATH, and a release build
(`cargo build --release`).

```sh
demo/asciinema_drive.sh /tmp/demo.cast
agg --idle-time-limit 2 --font-size 26 --theme asciinema /tmp/demo.cast demo/perl-lsp.gif
ffmpeg -i demo/perl-lsp.gif -pix_fmt yuv420p demo/perl-lsp.mp4
ffmpeg -i demo/perl-lsp.gif -c:v libvpx-vp9 -b:v 0 -crf 34 demo/perl-lsp.webm
```

## The scene

Three files: `app.pl` builds an account with `make_account(...)`, a helper
**default-exported** from `lib/Bank.pm`, which returns an `Account`
(`lib/Account.pm`, a Moo class). Five beats:

1. **Hover** `$acct` → `Account`, inferred *through* `make_account`'s return type
2. **Goto-def** `make_account` → `Bank.pm` (default-export resolution)
3. **Completion** `$acct->` → Account's methods (known via the function return)
4. **Rename** the `has balance` accessor in `Account.pm`
5. …which **cascades cross-file** into `Bank.pm`'s factory (`balance => …`)

## Recording must use a current binary + the demo root

Cross-file resolution depends on two things that have bitten this demo:

- **Build first** (`cargo build --release`). A stale binary silently lacks
  newer inference (e.g. a function's return type) and the cross-file beats fail.
- The driver runs nvim **from inside `demo/`** and `demo_init.lua` **pins the
  LSP root to `demo/`** — otherwise the root resolves to the outer repo (its
  `.git`), `use lib 'lib'` points at the wrong dir, and imports don't resolve.

The first take after a cold cache can miss the cross-file beats (the module
index lags attach); re-run — once warm it's stable. Sanity-check perl-lsp itself
synchronously with `perl-lsp --dump-package demo Bank` (look for
`bag_return_type: "Account"` on `make_account`).

## Determinism notes (why the config looks the way it does)

Scripted PTY input races the async LSP, so the recording fakes nothing but pins
timing-sensitive bits:

- **Completion menu** via `complete()` with the literal (verified) method list,
  invoked through `inoremap <C-l> <C-r>=…<CR>` — the textlock-safe form.
- **Caption** in the global tabline (a window-local winbar desyncs across `:b`).
- **Rename** calls `vim.lsp.buf.rename(newname)` directly (no `ui.input` prompt
  to mis-drive).
- The driver clears **both** files' swap files each run (a stale swap pops a
  prompt that turns subsequent keystrokes into buffer garbage).
