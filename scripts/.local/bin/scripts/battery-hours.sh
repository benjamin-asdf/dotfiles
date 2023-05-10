#!/bin/sh

upower -i $(upower -e | grep 'BAT') | grep 'time to empty\|time to full' | awk '{print $4, $5}'
