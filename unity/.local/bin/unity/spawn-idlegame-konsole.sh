#!/bin/sh

IDLEGAMEDIR=~/idlegame/IdleGame/
[ -n "$IDLEGAMEDIR" ] && rm -rf "$IDLEGAMEDIR/Temp/*"

xterm -T "idlegame-unity" -e sh -c "unity-editor -forcefree -buildTarget standalone -projectPath $IDLEGAMEDIR" &
