#!/bin/sh

git log --oneline

if [[ $(git log --oneline develop..$1 -- '*.prefab') ]];
   then echo "There are changes"
else
