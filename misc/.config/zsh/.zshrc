# adapted from Lukes zoomer shell


# Use powerline
USE_POWERLINE="true"

# Enable colors
autoload -U colors && colors
# PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]$mode_prompt%{$reset_color%}$%b "

HISTSIZE=10000
SAVEHIST=10000


# =============================== completions ================================
# before compinit

# nuke
_nuke_zsh_complete()
{
    local completions=("$(nuke :complete "$words")")
    reply=( "${(ps:\n:)completions}" )
}
compctl -K _nuke_zsh_complete nuke


# dotnet
# zsh parameter completion for the dotnet CLI
_dotnet_zsh_complete()
{
    local completions=("$(dotnet complete "$words")")

    reply=( "${(ps:\n:)completions}" )
}

compctl -K _dotnet_zsh_complete dotnet

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


# TODO fix bug where when you escape the prompt is not redrawn correctly
source "$HOME/.config/zsh/check-this.sh"


# # Change cursor shape for different vi modes.
# cursor_solid_beam='\e[6 q'
# cursor_solid_block='\e[2 q'
# function zle-keymap-select {
#     if [[ ${KEYMAP} == vicmd ]] ||
#            [[ $1 = 'block' ]]; then
#         echo -ne "$cursor_solid_block"
#     elif [[ ${KEYMAP} == main ]] ||
#              [[ ${KEYMAP} == viins ]] ||
#              [[ ${KEYMAP} = '' ]] ||
#              [[ $1 = 'beam' ]]; then
#         echo -ne "$cursor_solid_beam"
#     fi
# }
# zle -N zle-keymap-select
# zle-line-init() {
#     zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
#     echo -ne "$cursor_solid_beam"
# }
# zle -N zle-line-init
# echo -ne "$cursor_solid_beam" # Use beam shape cursor on startup.
# preexec() { echo -ne "$cursor_solid_beam" ;} # Use beam shape cursor for each new prompt.

# Vi modes curser and prompt.
# insert_mode_prompt="$fg[green]%}[I]"
# norm_mode_prompt="$fg[black]$bg[yellow]%}[N]"
# mode_prompt=$insert_mode_prompt
# cursor_solid_beam='\e[6 q'
# cursor_solid_block='\e[2 q'
# function zle-keymap-select {
#   if [[ ${KEYMAP} == vicmd ]] ||
#      [[ $1 = 'block' ]]; then
#       mode_prompt=$norm_mode_prompt
#       echo -ne "$cursor_solid_block"
#   elif [[ ${KEYMAP} == main ]] ||
#        [[ ${KEYMAP} == viins ]] ||
#        [[ ${KEYMAP} = '' ]] ||
#        [[ $1 = 'beam' ]]; then
#       mode_prompt=$insert_mode_prompt
#       echo -ne "$cursor_solid_beam"
#   fi
#   # whole prompt string
#   PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]$mode_prompt%{$reset_color%}$%b "
#   zle reset-prompt
# }

# zle -N zle-keymap-select
# zle-line-init() {
#     zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
#     echo -ne "$cursor_solid_beam"
# }
# zle -N zle-line-init
# echo -ne "$cursor_solid_beam" # Use beam shape cursor on startup.
# preexec() { echo -ne "$cursor_solid_beam" ;} # Use beam shape cursor for each new prompt.

# function zle-line-finish {
#     mode_prompt=$insert_mode_prompt
# }

# Use lf to switch directories and bind it to ctrl-o
# lfcd () {
#     tmp="$(mktemp)"
#     lf -last-dir-path="$tmp" "$@"
#     if [ -f "$tmp" ]; then
#         dir="$(cat "$tmp")"
#         rm -f "$tmp"
#         [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
#     fi
# }
# bindkey -s '^o' 'lfcd\n'

# Edit line in emacs with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

# Load aliases, shortcuts, dircolors, if existent.
[ -f "$HOME/.config/shortcutrc" ] && source "$HOME/.config/shortcutrc"
[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"
[ -f "$HOME/.config/zsh/dir-colors.zsh" ] && source "$HOME/.config/zsh/dir-colors.zsh"


# fzf
setopt HIST_IGNORE_ALL_DUPS # we do not want dups in our history
[-f /usr/share/fzf/fzf-extras.zsh ] && source /usr/share/fzf/fzf-extras.zsh
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
export FZF_DEFAULT_OPTS="--height 40% --layout=reverse --border"
export FZF_DEFAULT_COMMAND="fd --type f"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"


# zsh-syntax-highlighting. Should be last because it checks modules
# source "$HOME/repos/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# benj startup message, if interactive
# [[ $- == *i* ]] && hash best-message && echo Welcome to the benj zopanime shell. $(best-message)


# . "$GUIX_PROFILE/etc/profile"

eval `keychain -q --eval id_rsa`
