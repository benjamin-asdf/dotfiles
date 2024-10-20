#!/usr/bin/env sh
# Profile file. Runs on login.

export BROWSER=/usr/bin/qutebrowser

PATH="$HOME/.local/bin/pathoverride/:$PATH"

# Adds `~/.local/bin/` and all subdirectories to $PATH
PATH="$PATH:$(du "$HOME/.local/bin/" -L | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
PATH="$PATH:$(du "$HOME/.babashka" -L | cut -f2 | tr '\n' ':' | sed 's/:*$//')"
PATH="$PATH:$HOME/cognitect/datomic-cli/"
PATH="$PATH:$HOME/go/bin"

export PATH
export CHROME_EXECUTABLE="google-chrome-stable"

export SBCL_HOME=/usr/lib/sbcl/
# export SBCL_HOME="/home/benj/.local/sbcl/lib/sbcl/"

export ALTERNATE_EDITOR="vim"
export EDITOR="emacsclient"
export VISUAL="emacsclient"

# zsh
export ZDOTDIR="$HOME/.config/zsh/"

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

# shellcheck disable=SC1091
[ -n "$BASH_VERSION" ] && [ -f ~/.bashrc ] && . "$HOME/.bashrc"
