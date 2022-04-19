# Set default cursor.
source ~/.profile
setroot -cursor_name left_ptr
xset r rate 140 180

xautolock -time 4 -locker best-lock.sh &
setxkbmap -layout us -option ctrl:swapcaps_hyper,caps:hyper,nodeadkeys,nbsp:level3,altwin:menu_win
xcape -e 'Control_L=Escape'
redshift -l 55.7:12.6 -t 2200:1600 -m randr &
flameshot &
unclutter &
dunst &

/home/benj/repos/lisp/stumpwm/stumpwm
