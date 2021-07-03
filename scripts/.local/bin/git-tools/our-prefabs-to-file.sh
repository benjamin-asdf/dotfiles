#!/bin/sh

git diff-files --name-only --diff-filter=U -z | grep -z -e '\.prefab$' > conflicted-prefabs

sed -z -e 's|IdleGame/||' conflicted-prefabs | tr "\0" "\n"  > IdleGame/prefabs-for-rewrite.txt

xargs -a conflicted-prefabs -0 -I {} sh -c " git checkout --ours -- '{}' || true  && git add -- '{}' || true"
