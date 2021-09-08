#!/bin/sh


if [ "$1" == "-h" ]; then
    echo "AA both added
UU both modified
UD deleted by them
DU deleted by us
DD both deleted"
fi


git status --porcelain | rg "^$1\s+(.*)" -o -r '$1' | sed 's|\ |\\ |g'
