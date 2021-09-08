#!/usr/bin/env bash

link_name=$1

[ -z "$1" ] && echo provide link name && exit 1

assetsMounted=$(mount -l | grep "${link_name}/Assets")

symlink_dir=$COSDIR/$link_name

[ -z "$assetsMounted" ] && sudo mkdir -p "$symlink_dir"/Assets && sudo mount --bind "$IDLEGAMEDIR"/Assets "$symlink_dir"/Assets

spawn-idlegame-konsole.sh "$link_name" "$symlink_dir"
