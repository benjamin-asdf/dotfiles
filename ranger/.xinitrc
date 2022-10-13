source ~/.profile

# Fix java apps with tiling WMs
export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1
export GDK_CORE_DEVICE_EVENTS=1

xset r rate 140 220

setxkbmap -layout us -option ctrl:hyper_capscontrol,nodeadkeys,nbsp:level3,altwin:prtsc_rwin
xcape -e 'Control_L=Escape'

redshift -l 55.7:12.6 -t 2200:1600 -m randr &

unclutter &
dunst &

/home/benj/repos/stumpwm/stumpwm
