#!/bin/sh

# first arg is the rev you want to checkout files from
# second arg is a regex to filter files
# you probably want to to check with `git-file-grep-rev` first, which files you would checkout,
# then adapt your regex until you are satisfied with the file list.

cd "$(git-repo-root)" || exit 1

git ls-tree -z --name-only -r "$1" | grep "$2" -zZ | xargs -0 -I {} sh -c "git checkout $1 -- '{}'"
