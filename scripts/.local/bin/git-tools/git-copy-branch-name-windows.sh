#!/bin/sh

git branch --list | grep '*' | cut -c 3- | clip
