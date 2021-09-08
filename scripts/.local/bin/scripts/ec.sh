#!/bin/sh

emacsclient "$@" -s "$(fd . /run/user/1000/emacs/ | head -n1)"
