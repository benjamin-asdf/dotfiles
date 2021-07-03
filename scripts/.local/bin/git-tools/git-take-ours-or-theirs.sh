#!/bin/sh

whos="--ours"
[ -z $1 ] && whos="--theirs"

# read -p "Take $whos for all unmerged files, are you sure? (type y or Y)" -n 1 -r
# echo
# if [[ ! $REPLY =~ ^[Yy]$ ]]
#    then
#        exit 1
# fi


echo



gs=$(git status --porcelain)
echo $gs | rg "UD"
