#!/bin/sh

percentage=$(battery-percentage.sh)
hours_left=$(battery-hours.sh)
charging_status=$(cat /sys/class/power_supply/BAT0/status)

notify-send "Battery Overview" "[|||] Percentage: $percentage\nTime remaining: $hours_left\nCharging Status: $charging_status"
