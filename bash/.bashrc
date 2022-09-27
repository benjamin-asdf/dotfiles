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

# Enable history appending instead of overwriting.  #139609
shopt -s histappend

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

eval `keychain -q --eval id_rsa`

export ANDROID_HOME=$HOME/Android/Sdk
PATH=$PATH:$ANDROID_HOME/emulator
PATH=$PATH:$ANDROID_HOME/tools
PATH=$PATH:$ANDROID_HOME/tools/bin
PATH=$PATH:$ANDROID_HOME/platform-tools


alias deflate="perl -MCompress::Zlib -e 'undef $/; $\ = qq{\n}; print uncompress(<>)'"


_bb_complete() {
    BB_TASKS=$(bb tasks|bb -io '(->> *input* (drop 2) (map #(-> % (str/split #" ") first)))')
    BB_HELP=$(bb help|bb -io '(->> *input* (map #(->> % (re-find #"^  ([-a-z]+)") second)) (filter some?))')
    COMPREPLY=($(compgen -W "$BB_TASKS $BB_HELP" -- "${COMP_WORDS[$COMP_CWORD]}"))
}

complete -f -F _bb_complete bb
complete -W "$(bbin commands)" bbin
