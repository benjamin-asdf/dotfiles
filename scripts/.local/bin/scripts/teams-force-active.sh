#!/bin/sh

# teams-for-linux supports a state-file idle override (idleDetection.forceState
# in ~/.config/teams-for-linux/config.json). Writing "active" here keeps the
# presence status green regardless of actual system idle time. The app deletes
# this file on its own exit, so a systemd timer re-runs this script to recreate
# it after every restart.

echo active > "/tmp/teams-for-linux-idle-state-${USER}"
