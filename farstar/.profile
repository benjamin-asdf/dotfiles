#!/usr/bin/env sh
# Profile file. Runs on login.

export BROWSER=nyxt

# benj

# Adds `~/.local/bin/` and all subdirectories to $PATH
PATH="$PATH:$(du "$HOME/.local/bin/" | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
export PATH

export ALTERNATE_EDITOR="vim"
export EDITOR="emacsclient"
export VISUAL="emacsclient"

alias gs='git status'

export PASSWORD_STORE_DIR="$HOME/.local/.password-store"
# less/man colors
export LESS=-R
LESS_TERMCAP_mb="$(printf '%b' '[1;31m')"; a="${a%_}"
export LESS_TERMCAP_mb
LESS_TERMCAP_md="$(printf '%b' '[1;36m')"; a="${a%_}"
export LESS_TERMCAP_md
LESS_TERMCAP_me="$(printf '%b' '[0m')"; a="${a%_}"
export LESS_TERMCAP_me
LESS_TERMCAP_so="$(printf '%b' '[01;44;33m')"; a="${a%_}"
export LESS_TERMCAP_so
LESS_TERMCAP_se="$(printf '%b' '[0m')"; a="${a%_}"
export LESS_TERMCAP_se
LESS_TERMCAP_us="$(printf '%b' '[1;32m')"; a="${a%_}"
export LESS_TERMCAP_us
LESS_TERMCAP_ue="$(printf '%b' '[0m')"; a="${a%_}"
export LESS_TERMCAP_ue

# If we are running bash, source bashrc
[ -n "$BASH_VERSION" ] && [ -f ~/.bashrc ] && . "$HOME/.bashrc"

[ -e "$HOME/.guix-profile" ] \
    && . "$HOME/.guix-profile/etc/profile" \
    && . "$HOME/.config/guix/current/etc/profile"
