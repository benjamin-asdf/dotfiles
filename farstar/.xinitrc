#!/bin/sh

## WARNING: If started from TTY on a multi-seat, the user must run 'startx &
## vlock' to ensure the X locker cannot be bypassed.


userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

## Source environment config.
[ -f ~/.xprofile ] && . ~/.xprofile


if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi


# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi


## Run site init scripts. Usually not necessary.
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	  for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		    [ -x "$f" ] && . "$f"
	  done
	  unset f
fi

# Disable access control for the current user
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager
export _JAVA_AWT_WM_NONREPARENTING=1

# Start Shepherd to manage user daemons
if [ -z "$(pgrep -u daviwil shepherd)" ]; then
    shepherd
fi

# Run xsettingsd to progagate font and theme settings
xsettingsd &

# Enable screen compositing
compton &

# Turn off the system bell
xset -b

# Enable screen locking on suspend
xss-lock -- slock &

xset r rate 170 130

# Uncomment this to start xterm instead for debugging purposes!
# Then you can manually run the window manager and log output
# > exec dbus-launch emacs -mm --debug-init --use-exwm 2>&1 | tee ~/debug.log
#xterm

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init --use-exwm
