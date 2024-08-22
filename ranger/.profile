export BROWSER=/usr/bin/qutebrowser
export SBCL_HOME=/usr/lib/sbcl/

PATH="$PATH:$HOME/.babashka/bbin/bin"
PATH="$PATH:$(du "$HOME/.local/bin/" -L | cut -f2 | tr '\n' ':' | sed 's/:*$//')"

PATH="$HOME/.local/bin/pathoverride:$PATH"

export PATH

export ALTERNATE_EDITOR="vim"
export EDITOR="emacsclient"
export VISUAL="emacsclient"

export PASSWORD_STORE_DIR="$HOME/.local/.password-store"
