#!/bin/sh

# Wispr Flow's "Hub" window is created override-redirect (unmanaged by the
# WM) on Linux X11 -- confirmed upstream bug, wispr-flow-linux/wispr-flow-linux#36.
# It always spawns at the same on-screen position and can't be closed
# without crashing the whole Electron process (windowkill takes the GPU
# process down with it), so instead of closing it we just park it off
# the visible desktop. Idempotent: re-parking an already-parked window is a
# no-op, so this is safe to run on a tight timer.
#
# The "Status" window (recording indicator) has the same fixed-spawn-position
# bug, but it's meant to be visible, so instead of parking it off-screen we
# re-base it onto whichever monitor is currently primary. It always spawns
# at the same root-relative coordinates, which only land on the primary
# monitor if that monitor happens to start at 0,0.

primary_geom=$(xrandr --query 2>/dev/null | awk '/ primary /{print $4; exit}')
primary_x=$(echo "$primary_geom" | sed -n 's/^[0-9]*x[0-9]*+\([0-9]*\)+\([0-9]*\)$/\1/p')
primary_y=$(echo "$primary_geom" | sed -n 's/^[0-9]*x[0-9]*+\([0-9]*\)+\([0-9]*\)$/\2/p')
primary_w=$(echo "$primary_geom" | sed -n 's/^\([0-9]*\)x[0-9]*+[0-9]*+[0-9]*$/\1/p')

for id in $(xdotool search --class "wispr-flow" 2>/dev/null); do
	name=$(xdotool getwindowname "$id" 2>/dev/null)
	if [ "$name" = "Hub" ]; then
		xdotool windowmove "$id" -10000 -10000
	elif [ "$name" = "Status" ] && [ -n "$primary_x" ]; then
		eval "$(xdotool getwindowgeometry --shell "$id" 2>/dev/null)"
		if [ -n "$X" ] && { [ "$X" -lt "$primary_x" ] || [ "$X" -ge "$((primary_x + primary_w))" ]; }; then
			xdotool windowmove "$id" "$((primary_x + X))" "$Y"
		fi
	fi
done
