#!/bin/bash
# Record the perl-lsp demo by driving nvim inside a sized tmux pane and capturing
# the TERMINAL STREAM with asciinema (the pum is in the byte stream, so it's
# captured deterministically — no screenshot/GL race like vhs/kitty). agg then
# renders the .cast to gif headlessly.
#
#   demo/asciinema_drive.sh [out.cast]
set -u
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO"
CAST="${1:-/tmp/perl-lsp-demo.cast}"
export PATH="$HOME/miniconda3/envs/demotools/bin:/usr/bin:$PATH"

# Clean stale nvim swaps for the demo files — a leftover swap (from a prior
# take's nvim killed mid-rename) pops a prompt on `:e`, and the keystrokes after
# it cascade into the buffer as garbage.
rm -f "$HOME/.local/state/nvim/swap/"*app.pl.swp \
      "$HOME/.local/state/nvim/swap/"*Account.pm.swp \
      "$HOME/.local/state/nvim/swap/"*Bank.pm.swp 2>/dev/null
./target/release/perl-lsp --check "$REPO/demo" --severity warning >/dev/null 2>&1 || true

tmux kill-session -t rec 2>/dev/null; sleep 1
tmux new-session -d -s rec -x 150 -y 40
# asciinema records nvim INSIDE the 150x40 pane -> cast is 150x40. Run from
# demo/ (so `use lib 'lib'` -> demo/lib) with an absolute binary path.
tmux send-keys -t rec "cd '$REPO/demo' && PERL_LSP_BIN='$REPO/target/release/perl-lsp' asciinema rec --overwrite -c 'nvim -u demo_init.lua app.pl' '$CAST'" Enter
sleep 12  # asciinema + nvim + LSP attach + cross-file workspace resolution
          # (goto-def into Account.pm needs the module index, which lags attach).
          # The idle wait is compressed away by agg's --idle-time-limit.

S(){ tmux send-keys -t rec "$@"; }   # special keys (Escape/Enter/C-n…)
L(){ tmux send-keys -t rec -l "$@"; } # literal text

# Beat 1 — hover $acct: inferred Account, through the imported make_account()
L ';c'; sleep 0.3
L '6Gw'; sleep 0.5        # line 6, onto $acct
L 'K'; sleep 2.5
S Escape; sleep 0.8

# Beat 2 — goto-definition on make_account -> Bank.pm (the default-exported
# helper), then jump back. This is the cross-file import resolution.
L ';c'; sleep 0.3
L '/make_account'; S Enter; sleep 0.6   # onto make_account on line 6
L 'gd'; sleep 2.8                        # -> Bank.pm
S C-o; sleep 1.5                         # jump back to app.pl

# Beat 3 — completion on $acct-> then accept deposit. Account's methods are
# known even though $acct came from a cross-file function return.
L ';c'; sleep 0.3
L 'Go'; sleep 0.5
L '$acct->'; sleep 0.8
S C-l; sleep 2.0          # sync LSP fetch -> full method menu via complete()
L 'de'; sleep 1.5         # narrows the open menu natively to deposit/describe
S C-n; sleep 1.0          # select deposit (first)
S C-y; sleep 1.2          # accept
S Escape; sleep 0.8

# Beat 4 — rename the `has balance` accessor in Account.pm. Cascades within
# Account.pm (def + $self->balance calls) AND into Bank.pm's factory.
L ';c'; sleep 0.3
L ':e lib/Account.pm'; S Enter; sleep 1.5
L 'gg'; sleep 0.3
L '/has balance'; S Enter; sleep 0.6
L 'w'; sleep 0.5                     # cursor onto `balance`
L ';r'; sleep 2.5                    # direct rename -> available, cascades cross-file

# Beat 5 — switch to the (already-loaded, edited) Bank.pm buffer to show the
# cascade reached the factory: `balance => $balance` is now `available => ...`.
L ';c'; sleep 0.3
L ':b Bank.pm'; S Enter; sleep 0.7
L '/available'; S Enter; sleep 3.2

# end: quit nvim -> asciinema finalizes the cast
L ':qa!'; S Enter
sleep 3
tmux kill-session -t rec 2>/dev/null
echo "cast written:"; ls -la "$CAST"
