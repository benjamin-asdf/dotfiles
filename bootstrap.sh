#!/usr/bin/env bash
# Stow the active package set into $HOME. Idempotent.
#
# If a target file already exists and is not a symlink (e.g. a distro-provided
# ~/.bash_profile), it's moved aside to ~/<file>.pre-stow.bak so stow can take
# over. Existing correct symlinks are left alone.
#
# Usage:
#   ./bootstrap.sh             # stow active packages
#   ./bootstrap.sh -n          # dry run (pass through to stow)
#   ./bootstrap.sh foo bar     # stow just these packages

set -euo pipefail

cd "$(dirname "$(readlink -f "$0")")"

ACTIVE=(
    bash
    mememacs
    qutebrowser
    ranger
    scripts
    flameshot dunst mpv gtk dmenu
    clojure dot-clojure bb bbin .clj-kondo .lsp calva
    git-pack
)

# Optional packages — uncomment in your local install if you want them:
#   applications voidrice-scripts mail jetbrains unity nvim

stow_flags=()
packages=()
for arg in "$@"; do
    case "$arg" in
        -*) stow_flags+=("$arg") ;;
        *)  packages+=("$arg") ;;
    esac
done
[ ${#packages[@]} -eq 0 ] && packages=("${ACTIVE[@]}")

if ! command -v stow >/dev/null; then
    echo "error: GNU stow is not installed" >&2
    exit 1
fi

backup_conflicts() {
    local pkg="$1"
    # Ask stow to plan the install; parse its conflict reports and move
    # the offending real files aside.
    local out
    if out=$(stow -nv -t "$HOME" "$pkg" 2>&1); then
        return 0
    fi
    local conflict
    while IFS= read -r conflict; do
        [ -z "$conflict" ] && continue
        local target="$HOME/$conflict"
        if [ -e "$target" ] && [ ! -L "$target" ]; then
            local backup="$target.pre-stow.bak"
            echo "backing up $target -> $backup"
            mv -n -- "$target" "$backup"
        fi
    done < <(printf '%s\n' "$out" | sed -n -E 's|.*over existing target ([^ ]+) since.*|\1|p; s|.*existing target is [^:]+: (.*)|\1|p')
}

failed=()
for pkg in "${packages[@]}"; do
    if [ ! -d "$pkg" ]; then
        echo "skip $pkg (not a directory in repo)"
        continue
    fi
    backup_conflicts "$pkg"
    echo "stow $pkg"
    if ! stow -t "$HOME" "${stow_flags[@]}" "$pkg"; then
        failed+=("$pkg")
    fi
done

if [ ${#failed[@]} -gt 0 ]; then
    echo
    echo "FAILED: ${failed[*]}"
    echo "(stow printed the reason above each entry)"
    exit 1
fi
