#!/bin/sh

echo "hi" > ~/hi-bro

# Source .profile for common environment vars
# . ~/.profile

# Disable access control for the current user
# xhost +SI:localuser:$USER

# Start Shepherd to manage user daemons
if [ -z "$(pgrep -u benj shepherd)" ]; then
 #   shepherd
fi

# Run xsettingsd to progagate font and theme settings
# xsettingsd &

# Enable screen compositing
# compton &

# Uncomment this to start xterm instead for debugging purposes!
# Then you can manually run the window manager and log output
#> exec dbus-launch emacs -mm --debug-init --use-exwm 2>&1 | tee ~/debug.log
xterm

# Fire it up
#exec dbus-launch --exit-with-session emacs -mm --debug-init
## `exwm-enable' has to be called before the frame is spawned.
#emacs --daemon --eval "(require 'exwm)" -f exwm-enable
# exec emacsclient -c
#--use-exwm

exec "$@"
