#!/bin/sh

# ln -s ~/.xinitrc ~/exwm/start-exwm.sh
# Set the screen DPI (uncomment this if needed!)
# xrdb ~/.emacs.d/exwm/Xresources

# Run the screen compositor
# compton &

# Source .profile for common environment vars
. ~/.profile

# Disable access control for the current user
xhost +SI:localuser:$USER

# Enable screen locking on suspend
# xss-lock -- slock &

# Start Shepherd to manage user daemons
if [ -z "$(pgrep -u benj shepherd)" ]; then
  shepherd
fi

redshift -O 2400

eval `ssh-agent -s`

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init
