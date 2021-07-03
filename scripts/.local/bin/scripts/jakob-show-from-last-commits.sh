#!/bin/sh

gc | grep "$1" | awk '{print $2}' | xargs git show

# you have to put it in your PATH and make it executable
# then you can do jakob-show-from-last-commits.sh "some string"
