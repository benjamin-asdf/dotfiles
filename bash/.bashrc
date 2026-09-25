#
# ~/.bashrc
#

[[ $- != *i* ]] && return

__prompt_command() {
    PS1="[$?] \$ "
}

PROMPT_COMMAND=__prompt_command

alias more=less

xhost +local:root > /dev/null 2>&1

# Bash won't get SIGWINCH if another process is in the foreground.
# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize
shopt -s expand_aliases
shopt -s histappend

shopt -s globstar

#
# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# keychain agent vars are exported once per login by .bash_profile; here we
# just pick up the cached env so non-login interactive shells (terminal
# emulators spawned by the WM) inherit SSH_AUTH_SOCK without re-running
# keychain on every prompt.
[ -f "$HOME/.keychain/$HOSTNAME-sh" ] && . "$HOME/.keychain/$HOSTNAME-sh" >/dev/null

append_path() {
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        export PATH="$PATH:$1"
    fi
}

export ANDROID_HOME=$HOME/Android/Sdk
append_path "$ANDROID_HOME/emulator"
append_path "$ANDROID_HOME/tools"
append_path "$ANDROID_HOME/tools/bin"
append_path "$ANDROID_HOME/platform-tools"

alias deflate="perl -MCompress::Zlib -e 'undef $/; $\ = qq{\n}; print uncompress(<>)'"

# Lazy bb completion: the heavy work (running `bb tasks` / `bb help`) only
# happens the first time Tab is pressed on `bb`, so a broken bb.edn in cwd
# at shell startup can't poison the prompt.
_bb_complete_real() {
    local cur="${COMP_WORDS[$COMP_CWORD]}"
    local bb_tasks bb_help
    bb_tasks=$(bb tasks 2>/dev/null | bb -io '(->> *input* (drop 2) (map #(-> % (str/split #" ") first)))' 2>/dev/null)
    bb_help=$(bb help 2>/dev/null | bb -io '(->> *input* (map #(->> % (re-find #"^  ([-a-z]+)") second)) (filter some?))' 2>/dev/null)
    COMPREPLY=($(compgen -W "$bb_tasks $bb_help" -- "$cur"))
}
_bb_complete_init() {
    complete -f -F _bb_complete_real bb
    _bb_complete_real "$@"
}
complete -f -F _bb_complete_init bb

a() {
    [[ -f ./activate.sh ]] && source ./activate.sh
    [[ -f ./venv/bin/activate ]] && source ./venv/bin/activate
}

alias c='claude --dangerously-skip-permissions'
alias pc='CLAUDE_CONFIG_DIR=~/.claude-personal claude --dangerously-skip-permissions'

# pnpm
export PNPM_HOME="/home/benj/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end
