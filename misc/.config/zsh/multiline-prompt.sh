

# For the people having problems using reset-prompt with multiline prompts, this worked for me: http://zeitlens.com/posts/2014-06-29-howto-zsh-vi-style.html In combination with https://stackoverflow.com/questions/3622943/zsh-vi-mode-status-line I ended up doing:

terminfo_down_sc=$terminfo[cud1]$terminfo[cuu1]$terminfo[sc]$terminfo[cud1]

function insert-mode () { echo "-- INSERT --" }
function normal-mode () { echo "-- NORMAL --" }

precmd () {
    # yes, I actually like to have a new line, then some stuff and then
    # the input line
    # print -rP "%n~%{$fg[blue]%}%m%{$reset_color%}"

    # this is required for initial prompt and a problem I had with Ctrl+C or
    # Enter when in normal mode (a new line would come up in insert mode,
    # but normal mode would be indicated)
    PS1="[%n~%{$fg[blue]%}%m%{$reset_color%}]%{$terminfo_down_sc$(insert-mode)$terminfo[rc]%}%~ $ "
}
function set-prompt () {
    case ${KEYMAP} in
      (vicmd)      VI_MODE="$(normal-mode)" ;;
      (main|viins) VI_MODE="$(insert-mode)" ;;
      (*)          VI_MODE="$(insert-mode)" ;;
    esac
    PS1="%{$terminfo_down_sc$VI_MODE$terminfo[rc]%}%~ $ "
}

function zle-line-init zle-keymap-select {
    set-prompt
    zle reset-prompt
}
preexec () { print -rn -- $terminfo[el]; }

zle -N zle-line-init
zle -N zle-keymap-select
