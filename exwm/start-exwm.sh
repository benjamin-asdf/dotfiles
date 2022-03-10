# Source .profile for common environment vars
. ~/.profile

# Disable access control for the current user.
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Set default cursor.
xsetroot -cursor_name left_ptr

# Enable screen locking on suspend
# xss-lock -- slock &

# Uncomment the following block to use the exwm-xim module.
#export XMODIFIERS=@im=exwm-xim
#export GTK_IM_MODULE=xim
#export QT_IM_MODULE=xim
#export CLUTTER_IM_MODULE=xim

xset r rate 160 140
setxkbmap -layout us -option ctrl:swapcaps,nodeadkeys,nbsp:level3,altwin:prtsc_rwin
xcape -e 'Control_L=Escape'
redshift -l 55.7:12.6 -t 2200:1600 -m randr &
flameshot &
unclutter &

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init
