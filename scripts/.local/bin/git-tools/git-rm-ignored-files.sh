#!/bin/sh

# remove all ignored files from current dir.
# this only adds the deletion of files so there is serenity about modfied files


# delete everything from the index, then add everything. Trick to easily get the deletion of ignored files
git rm -r --cached . && git add .

gs-files.sh .* | sort >> all-modified
gs-files.sh D | sort >> all-deleted
modified=$(comm -23 all-modified all-deleted)

pushd .
cd $(git rev-parse --show-toplevel)
xargs git reset -- <<< $modified
popd

rm all-modified all-deleted
