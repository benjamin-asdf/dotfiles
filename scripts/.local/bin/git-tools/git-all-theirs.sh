#!/bin/sh

gs-files-grep '^[UDA][UA] ' | xargs --null -I {} sh -c "git checkout --theirs -- '{}' && git add -- '{}'"

gs-files-grep '^AU ' | xargs --null -I {} sh -c "rm '{}' && git add -- '{}'"

gs-files-grep '^DD ' | xargs --null -I {} sh -c "git add -- '{}'"
