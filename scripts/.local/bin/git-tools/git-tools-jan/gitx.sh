#!/usr/bin/env bash

source jit.sh

git_command=$1
shift;
jit $git_command "*." "" $*
