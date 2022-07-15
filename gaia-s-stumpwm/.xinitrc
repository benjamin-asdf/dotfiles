source ~/.profile


# Fix java apps with tiling WMs
export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1
#wmname LG3D
export GDK_CORE_DEVICE_EVENTS=1


# Set default cursor.
setroot -cursor_name left_ptr
# The  first  specifies  the  delay before autorepeat starts and the second specfies the repeat rate.
xset r rate 120 240

xautolock -time 4 -locker best-lock.sh &
setxkbmap -layout us -option ctrl:swapcaps_hyper,caps:hyper,nodeadkeys,nbsp:level3,altwin:menu_win
xcape -e 'Control_L=Escape'
redshift -l 55.7:12.6 -t 2200:1600 -m randr &
flameshot &
unclutter &
dunst &

/home/benj/repos/lisp/stumpwm/stumpwm
