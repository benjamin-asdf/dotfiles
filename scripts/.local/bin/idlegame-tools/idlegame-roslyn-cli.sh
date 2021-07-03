#!/bin/sh

export CUSTOM_MSBUILD_PATH=/usr/lib/mono/msbuild/15.0/bin

cmd="$IDLEGAMEDIR/Tools/RoslynAnalyzers/EntityClosureCLI/EntityClosureCLI.exe"

$cmd -s  -t Playground
