source ~/.profile

# export XDG_SESSION_TYPE=X11

# Fix java apps with tiling WMs
export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1
#wmname LG3D
export GDK_CORE_DEVICE_EVENTS=1

# Set default cursor.
setroot -cursor_name left_ptr
# The  first  specifies  the  delay before autorepeat starts and the second specfies the repeat rate.
xset r rate 120 250

# https://wiki.archlinux.org/title/Xrandr
# used xlayoutdisplay
# xrandr \
#  --dpi 96 \
#  --output DVI-D-0 --mode 1920x1080 --rate 60 --pos 0x0  \
#  --output HDMI-0 --mode 1920x1080 --rate 60 --pos 1920x0 \
#  --output DP-0 --mode 2560x1440 --rate 60 --pos 3840x0 --primary \
#  --output DP-1 --off

# 3 monitors
# xrandr --output DVI-D-0 --primary --mode 2560x1440 --pos 1920x0 --rotate normal --output HDMI-0 --mode 1920x1080 --pos 4480x0 --rotate normal --output DP-0 --mode 1920x1080 --pos 0x0 --rotate normal --output DP-1 --off

# middle and right
# xrandr --output DVI-D-0 --off --output HDMI-0 --mode 1920x1080 --pos 4480x0 --rotate normal --output DP-0 --primary --mode 2560x1440 --pos 1920x0 --rotate normal --output DP-1 --off


xautolock -time 4 -locker i3lock &

setxkbmap -layout us -option ctrl:hyper_capscontrol,nodeadkeys,nbsp:level3,altwin:menu_win


# xev | awk -F'[ )]+' '/^KeyPress/ { a[NR+2] } NR in a { printf "%-3s %s\n", $5, $8 }'

# orig:
# // Make the left Ctrl key a left Hyper,
# // and the CapsLock key a left Control.
# partial modifier_keys
# xkb_symbols "hyper_capscontrol" {
#     replace key <CAPS> { [ Control_L ], type[group1] = "ONE_LEVEL" };
#     replace key <LCTL> { [ Hyper_L ] };
#     modifier_map Control { <CAPS> };
#     modifier_map Mod3    { <LCTL> };
# };

# /usr/share/X11/xkb/symbols/ctrl

# modified to what i neede for kinesis
# partial modifier_keys
# xkb_symbols "hyper_capscontrol" {
#     replace key <CAPS> { [ Super_L ], type[group1] = "ONE_LEVEL" };
#     replace key <LCTL> { [ Hyper_L ] };
#     modifier_map Mod4 { <CAPS> };
#     modifier_map Mod3    { <LCTL> };
# };

xmodmap ~/.Xmodmap

# make escape a control  

spare_modifier="Hyper_R"
xmodmap -e "keycode 9 = $spare_modifier"
xmodmap -e "remove mod4 = $spare_modifier"
xmodmap -e "add Control = $spare_modifier"
xmodmap -e "keycode 93 = Escape"
xcape -e "$spare_modifier=Escape"

# make ; a cohtrol when pressed
# eh has downsides and I want shift ; to be colon
# spare_modifier="Alt_R"
# xmodmap -e "keycode 47 = $spare_modifier"
# xmodmap -e "remove mod1 = $spare_modifier"
# xmodmap -e "add Control = $spare_modifier"
# xmodmap -e "keycode 230 = semicolon"
# xcape -e "$spare_modifier=semicolon"

redshift -l 55.7:12.6 -t 2600:1800 -m randr &
flameshot &
unclutter &
dunst &

xrdb -merge ~/.Xresources

exec /home/benj/repos/stumpwm/stumpwm
