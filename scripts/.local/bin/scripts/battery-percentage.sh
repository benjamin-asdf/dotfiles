#!/bin/sh

upower -i $(upower -e | grep 'BAT') | grep 'percentage' | awk '{print $2}'
