# adapted from Lukes zoomer shell

# Use powerline
USE_POWERLINE="true"

# Enable colors
autoload -U colors && colors

HISTSIZE=10000
SAVEHIST=10000


# =============================== completions ================================
# before compinit

# fzf
fpath=(/usr/share/fzf/completion.zsh $fpath)

autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist

# on slow systems, only check if we need to update dump file once per day
# https://gist.github.com/ctechols/ca1035271ad134841284#gistcomment-2308206
if [ $SLOW_SYSTEM ]; then
    setopt EXTENDEDGLOB
    for dump in ${ZDOTDIR:-$HOME}/.zcompdump(#qN.m1); do
        compinit
        if [[ -s "$dump" && (! -s "$dump.zwc" || "$dump" -nt "$dump.zwv" ) ]]; then
            zcompile "$dump"
        fi
    done
    unsetopt EXTENDEDGLOB
    compinit -C
    else # quick system
        compinit
fi

_comp_options+=(globdots)		# Include hidden files.
# ==============================================================================

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# M-. emacs style
bindkey -v '^[.' insert-last-word

source "$HOME/.config/zsh/prompt.sh"

# Edit line in emacs with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

# Load aliases, shortcuts, dircolors, if existent.
[ -f "$HOME/.config/zsh/dir-colors.zsh" ] && source "$HOME/.config/zsh/dir-colors.zsh"

# fzf
setopt HIST_IGNORE_ALL_DUPS # we do not want dups in our history
[ -f /usr/share/fzf/fzf-extras.zsh ] && source /usr/share/fzf/fzf-extras.zsh
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border"
export FZF_DEFAULT_COMMAND="fd --type f"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"

# . "$GUIX_PROFILE/etc/profile"

_bb_tasks() {
    local matches=(`bb tasks |tail -n +3 |cut -f1 -d ' '`)
    compadd -a matches
    _files # autocomplete filenames as well
}
compdef _bb_tasks bb

function _bbin() { _arguments "1: :($(bbin commands))" }
compdef _bbin bbin

eval `keychain -q --eval id_rsa`

# set the window title to last command
# https://tldp.org/HOWTO/Xterm-Title-4.html

case $TERM in
    xterm*)
      preexec () {
        print -Pn "\e]0;xterm - $* %~\a"
      }
    ;;
esac
