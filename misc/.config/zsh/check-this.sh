

# For the people having problems using reset-prompt with multiline prompts, this worked for me: http://zeitlens.com/posts/2014-06-29-howto-zsh-vi-style.html In combination with https://stackoverflow.com/questions/3622943/zsh-vi-mode-status-line I ended up doing:

terminfo_down_sc=$terminfo[cud1]$terminfo[cuu1]$terminfo[sc]$terminfo[cud1]

function insert-mode () { echo "%F{green}[ INSERT ]%f " }
function normal-mode () { echo "%F{yellow}[ NORMAL ]%f " }

# TODO I want the prompt in one line and some colors and formatting
precmd () {
    # yes, I actually like to have a new line, then some stuff and then
    # the input line
    # %F{green}[%?]%f
    print -rP "
[%?]%d@%n~%{$fg[cyan]%}%m%{$reset_color%}"

    # this is required for initial prompt and a problem I had with Ctrl+C or
    # Enter when in normal mode (a new line would come up in insert mode,
    # but normal mode would be indicated)
    PS1="%{$terminfo_down_sc$(insert-mode)$terminfo[rc]%}%~ $ "
}
function set-prompt () {
    case ${KEYMAP} in
      (vicmd)      VI_MODE="$(normal-mode)" ;;
      (main|viins) VI_MODE="$(insert-mode)" ;;
      (*)          VI_MODE="$(insert-mode)" ;;
    esac
    PS1="%{$terminfo_down_sc$VI_MODE$terminfo[rc]%}%~ $ "
}

cursor_solid_beam='\e[6 q'
cursor_solid_block='\e[2 q'

function zle-line-init zle-keymap-select {
    set-prompt
    zle reset-prompt
    if [[ ${KEYMAP} == vicmd ]] ||
           [[ $1 = 'block' ]]; then
        echo -ne "$cursor_solid_block"
    elif [[ ${KEYMAP} == main ]] ||
             [[ ${KEYMAP} == viins ]] ||
             [[ ${KEYMAP} = '' ]] ||
             [[ $1 = 'beam' ]]; then
        echo -ne "$cursor_solid_beam"
    fi
}
preexec () { print -rn -- $terminfo[el]; echo -ne "$cursor_solid_beam" ; }

zle -N zle-line-init
zle -N zle-keymap-select
