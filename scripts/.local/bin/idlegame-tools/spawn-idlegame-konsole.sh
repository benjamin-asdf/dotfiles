#!/bin/sh


unity_args="-forcefree -buildTarget standalone -projectPath $2"
# -CacheServerIPAddress 192.168.2.113:80

echo unity args are: "$unity_args"

xterm -T "$1" -e sh -c "unity-editor $unity_args" &
