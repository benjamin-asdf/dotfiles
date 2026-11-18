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
#   ./bootstrap.sh --check     # change nothing, report what is off

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

# The host package (host-<hostname>, when the repo has one) holds what differs
# per machine: dpi, memory caps, a StumpWM host.lisp.
host_pkg="host-$(hostname)"
[ -d "$host_pkg" ] && ACTIVE+=("$host_pkg")

# Host packages are stowed file by file: systemd ignores a drop-in directory
# (jvm.slice.d) that is itself a symlink.
pkg_flags() {
    case "$1" in host-*) echo --no-folding ;; esac
}

stow_flags=()
packages=()
check=""
for arg in "$@"; do
    case "$arg" in
        --check) check=1 ;;
        -*) stow_flags+=("$arg") ;;
        *)  packages+=("$arg") ;;
    esac
done
[ ${#packages[@]} -eq 0 ] && packages=("${ACTIVE[@]}")

if ! command -v stow >/dev/null; then
    echo "error: GNU stow is not installed" >&2
    exit 1
fi

check() {
    local problems=0 pkg out missing link cmd unit foreign
    ok()  { printf '  ok    %s\n' "$1"; }
    bad() { printf '  FAIL  %s\n' "$1"; problems=$((problems + 1)); }

    echo "stow packages"
    for pkg in "${packages[@]}"; do
        if [ ! -d "$pkg" ]; then
            bad "$pkg: not a directory in the repo"
        elif ! out=$(stow -nv -t "$HOME" $(pkg_flags "$pkg") "$pkg" 2>&1); then
            # a target linked into the package by hand (absolute link) is fine
            foreign=0
            while IFS= read -r link; do
                case "$(readlink -f "$HOME/$link")" in
                    "$PWD/$pkg"/*) ;;
                    *) foreign=$((foreign + 1)) ;;
                esac
            done < <(printf '%s\n' "$out" | sed -n -E 's|.*existing target is [^:]+: (.*)|\1|p')
            if [ "$foreign" -gt 0 ] || ! printf '%s\n' "$out" | grep -q 'existing target is'; then
                bad "$pkg: $(printf '%s\n' "$out" | grep -c '^  \* ') stow conflicts (stow -nv -t ~ $pkg)"
            else
                ok "$pkg (linked by hand)"
            fi
        else
            missing=$(printf '%s\n' "$out" | grep -c '^LINK: ' || true)
            if [ "$missing" -gt 0 ]; then
                bad "$pkg: $missing links missing (./bootstrap.sh $pkg)"
            else
                ok "$pkg"
            fi
        fi
    done

    echo "dangling links into the repo"
    out=$(find "$HOME" -maxdepth 5 -xtype l -lname '*dotfiles/*' \
               -not -path '*/node_modules/*' -not -path "$PWD/*" 2>/dev/null || true)
    if [ -n "$out" ]; then
        while IFS= read -r link; do bad "$link"; done <<<"$out"
    else
        ok "none"
    fi
    # stumpish points into the StumpWM contrib modules, not into the repo
    if [ -x "$HOME/.local/bin/stumpish" ]; then ok "stumpish"; else bad "stumpish: ~/.stumpwm.d/modules is missing"; fi

    echo "commands the config calls"
    for cmd in stumpwm emacs emacsclient qutebrowser xterm dmenu dunst flameshot \
               bb clojure pass clipmenud xautolock redshift unclutter slop ffmpeg mpv; do
        if command -v "$cmd" >/dev/null; then ok "$cmd"; else bad "$cmd: not installed"; fi
    done

    echo "fonts"
    if [ -n "$(fc-list 'Fira Code' 2>/dev/null)" ]; then ok "Fira Code"; else bad "Fira Code: not installed"; fi
    if compgen -G '/usr/share/fonts/misc/ter-*' >/dev/null; then
        ok "Terminus (X core font)"
    else
        bad "Terminus: install terminus-font (StumpWM falls back to fixed)"
    fi

    echo "user units"
    for unit in ranger/.config/systemd/user/*.service ranger/.config/systemd/user/*.timer; do
        unit=${unit##*/}
        if systemctl --user is-failed --quiet "$unit" 2>/dev/null; then
            bad "$unit: failed (systemctl --user status $unit)"
        else
            ok "$unit"
        fi
    done

    echo "palette"
    if ! command -v bb >/dev/null; then
        bad "palette: bb is missing, cannot check"
    elif out=$(bb palette.bb --check 2>&1); then
        ok "$(printf '%s\n' "$out" | tail -1)"
    else
        bad "$(printf '%s\n' "$out" | tail -1) (bb palette.bb fixes drift)"
    fi

    echo
    if [ "$problems" -gt 0 ]; then
        echo "$problems problem(s)"
        return 1
    fi
    echo "all good"
}

if [ -n "$check" ]; then
    check
    exit
fi

backup_conflicts() {
    local pkg="$1"
    # Ask stow to plan the install; parse its conflict reports and move
    # the offending real files aside.
    local out
    if out=$(stow -nv -t "$HOME" $(pkg_flags "$pkg") "$pkg" 2>&1); then
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
    if ! stow -t "$HOME" "${stow_flags[@]}" $(pkg_flags "$pkg") "$pkg"; then
        failed+=("$pkg")
    fi
done

# GTK 4 / libadwaita / Electron read the dark preference from gsettings,
# not from gtk/.config/gtk-3.0/settings.ini.
if [[ " ${packages[*]} " == *" gtk "* ]] && [[ " ${stow_flags[*]} " != *" -n "* ]] \
       && command -v gsettings >/dev/null; then
    gsettings set org.gnome.desktop.interface color-scheme prefer-dark
    gsettings set org.gnome.desktop.interface gtk-theme Adwaita-dark
    gsettings set org.gnome.desktop.interface cursor-theme Adwaita
    gsettings set org.gnome.desktop.interface font-name 'Noto Sans 10'
    gsettings set org.gnome.desktop.interface monospace-font-name 'Fira Code 10'
fi

if [ ${#failed[@]} -gt 0 ]; then
    echo
    echo "FAILED: ${failed[*]}"
    echo "(stow printed the reason above each entry)"
    exit 1
fi
