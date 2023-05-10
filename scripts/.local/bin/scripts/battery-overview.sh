#!/bin/sh

percentage=$(battery-percentage.sh)
hours_left=$(battery-hours.sh)
notify-send "Battery Overview" "[|||] Percentage: $percentage\nTime remaining: $hours_left"
