#!/bin/sh

# Re-run keyboard-setup.sh whenever a keyboard is plugged in (the Kinesis,
# mostly). A hotplugged keyboard starts with the server's default keymap and
# repeat rate, so CapsLock-as-Control, Hyper, xcape's Escape and the fast
# autorepeat are all gone until keyboard-setup.sh runs again.
#
# Runs as keyboard-hotplug.service under stumpwm-session.target (needs DISPLAY
# from the session's dbus-update-activation-environment). Listens to udev's
# input events; one plug produces a burst of them, so wait for the burst to
# settle and run the setup once.

## stdbuf: line-buffer udevadm's output into the pipe, so an event isn't held
## back in a block buffer until more events arrive.
stdbuf -oL udevadm monitor --udev --subsystem-match=input | while read -r line; do
  case $line in
    *" add "*)
      ## Drain the rest of the burst (read times out after 1 s of quiet), then
      ## give X a moment to attach the new device.
      while read -r -t 1 _; do :; done
      sleep 0.5
      echo "input device added, running keyboard-setup.sh"
      keyboard-setup.sh
      ;;
  esac
done
