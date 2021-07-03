#!/usr/bin/env bash

git diff --check --cached | grep -B1 '' | grep '\bfile:' | sort | uniq
