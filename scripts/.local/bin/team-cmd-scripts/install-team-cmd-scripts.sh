#!/bin/sh

# run this ones to add this folder to your path.
# it will make it so you can run the scripts from anywhere

pwd | grep -q '\/TeamCommands\/scripts$' || (echo "Enter TeamCommand/scripts folder first." && exit 1)

printf "\nexport COSDIR=\"C:/ClashOfStreamers\"" >> "$HOME/.bashrc"
printf "\nPATH=\"\$PATH:%s\"\nexport PATH" "$(pwd)" >> "$HOME/.bashrc" && . "$HOME/.bashrc"
printf "Added %s to your path in %s. You might want to adjust this file.\n" "$(pwd)" "$HOME/.bashrc"

[ -d "$COSDIR" ] || printf "%s is not a directory. This looks like a wrong setup. Fix the path." "$COSDIR"
