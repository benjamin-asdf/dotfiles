# dotfiles

A maximalist Lisp hacker's environment. StumpWM (Common Lisp) WM, custom
Emacs distro ("mememacs"), Clojure/Babashka for code and scripting, bash
for shell. See `readme.org` for the full philosophy.

## Layout: GNU Stow packages

Each top-level dir is a Stow package — its contents are symlinked into
`$HOME` from the repo. Hidden packages (`.clj-kondo`, `.lsp`) are real
packages too, just dotfile-named.

Active packages (stowed on the current machine):

- `bash` — `.bashrc` + `.bash_profile`. Login shell sets up keychain
  once; interactive shells inherit env via `~/.keychain/$HOSTNAME-sh`.
- `mememacs` — personal Emacs distro. Not Doom, not Spacemacs.
- `qutebrowser` — config + bookmarks. Bound to feed URLs back into
  Emacs (org-protocol, kill ring).
- `ranger` — StumpWM init for the "ranger" laptop. Lives at
  `ranger/.stumpwm.d/init.lisp`. The top-level `stumpwm/` dir is
  retired (empty `.stumpwm.d`); always edit `ranger/.stumpwm.d/`.
- `scripts` — see "Scripts" below.
- `flameshot`, `dunst`, `mpv`, `gtk`, `dmenu` — desktop apps.
- `clojure`, `dot-clojure` (git submodule), `bb`, `bbin`, `.clj-kondo`,
  `.lsp`, `calva` — Clojure toolchain.
- `git-pack` — shared git config.

Retired packages kept for reference (do not edit, do not stow):
`nyxt`, `exwm`, `i3`, `zsh`, `farstar`, `gaia-s`, `gaia-s-stumpwm`,
top-level `stumpwm` (empty — superseded by `ranger/.stumpwm.d/`).

`bootstrap.sh` at the repo root stows the active set in one go. The
package list it uses is the source of truth; if you add or retire a
package, update both `bootstrap.sh` and `readme.org`.

## Scripts

The `scripts` package is unusually nested. After `stow scripts`:

- `~/.local/bin/` — small/recent additions (`stumpish`,
  `mcp-remote-node20`, `parinfer-rust`, `pathoverride/`).
- `~/.local/bin/scripts/` — ~80 older scripts (`record-*`, `s3-cos-*`,
  `git-lfs-*`, etc.). Both dirs are on `$PATH` (see `.bashrc`).

`stumpish` in the top-level dir is a *symlink* into
`~/.stumpwm.d/modules/util/stumpish/stumpish`, so it requires the
StumpWM contrib modules to be installed at that path.

## Shell quirks worth knowing

- `bb` Tab completion is lazy (`_bb_complete_init` rebinds itself to
  the real completer on first use). This is deliberate — running
  `bb tasks` at every shell startup against a broken `bb.edn` was
  poisoning prompts.
- `a` is a function (not an alias) that sources `./activate.sh` or
  `./venv/bin/activate` if present. Aliases can't span lines.
- `c` aliases `claude --dangerously-skip-permissions`.

## Interacting with the running Emacs

A user Emacs daemon is running — `emacsclient --eval '(sexp)'` reaches
it. **After editing any elisp file, always reload it via emacsclient:**

    emacsclient --eval '(load-file "/home/benj/dotfiles/mememacs/.emacs-mememacs.d/lisp/init-project.el")'

`emacsclient --eval` returns the printed form of the result (`t`,
`nil`, a quoted string, etc.) — handy for verifying a `defun` landed
(`(fboundp 'mm/foo)` → `t`). For multi-step reloads chain forms with
`(progn ...)`.

## Editing this repo

- Treat `readme.org` and this file as both reflecting reality. If you
  change package status (active ⇄ retired), update both.
- Don't rewrite the retired packages. Ask before deleting them — the
  user keeps them on purpose for reference.
- Stow conflicts: if `~/.bash_profile` (or any target) already exists
  unsymlinked, move it aside (`mv ~/.bash_profile ~/.bash_profile.pre-stow.bak`)
  rather than `--adopt`-ing — `--adopt` overwrites repo content with
  the existing file.
