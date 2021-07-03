#!/usr/bin/env bash

source jit.sh

gitCommand=$1
shift;
jit $gitCommand "*/" ".*" $*
