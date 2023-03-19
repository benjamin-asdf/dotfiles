source ~/.profile

gsettings set org.gnome.desktop.interface color-scheme prefer-dark
gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"

# Fix java apps with tiling WMs
export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1
#wmname LG3D
export GDK_CORE_DEVICE_EVENTS=1

# Set default cursor.
setroot -cursor_name left_ptr
# The  first  specifies  the  delay before autorepeat starts and the second specfies the repeat rate.
xset r rate 120 250

# xautolock -time 4 -locker best-lock.sh &
setxkbmap -layout us -option ctrl:hyper_capscontrol,nodeadkeys,nbsp:level3,altwin:menu_win

# https://askubuntu.com/questions/63681/how-can-i-make-xrandr-customization-permanent
# I can use lightdm or xorg.confgs
xrandr --output HDMI-0 --left-of DVI-D-0 --auto
xrandr --output DP-0 --left-of HDMI-0 --auto

xcape -e 'Control_L=Escape'
redshift -l 55.7:12.6 -t 2600:1800 -m randr &
flameshot &
unclutter &
dunst &

/home/benj/repos/lisp/stumpwm/stumpwm

# xset r rate 200 180
