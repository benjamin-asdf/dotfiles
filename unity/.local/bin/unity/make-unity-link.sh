#!/bin/sh

[ -z "$1" ] && echo "first arg is the target project" && exit 1
[ -z "$2" ] && echo "second arg is the destination path" && exit 1

absPathDirname()
{
    [ -d "${1}" ] && set -- "${1}" || set -- "`dirname "${1}"`" "/`basename "${1}"`"
# shellcheck disable=SC2164
    echo "`cd "${1}"; pwd`${2}";
}

PROJ=$(absPathDirname "$1")
DEST_DIR=$(absPathDirname "$2")

mkdir -p "$DEST_DIR"

ln -sf "$PROJ/Config" "$DEST_DIR/Config"
ln -sf "$PROJ/ProjectSettings" "$DEST_DIR/ProjectSettting"
ln -sf "$PROJ/Packages" "$DEST_DIR/Packages"
ln -sf "$PROJ/.genignore" "$DEST_DIR/.genignore"
ln -sf "$PROJ/Assets" "$DEST_DIR/Assets"

rm "$DEST_DIR/Packages/packages-lock.json"
