source ~/.profile

# Fix java apps with tiling WMs
export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1
export GDK_CORE_DEVICE_EVENTS=1

setroot -cursor_name left_ptr

xset r rate 140 220

xautolock -time 4 -locker i3lock &

# setxkbmap -layout us -option ctrl:hyper_capscontrol,nodeadkeys,nbsp:level3,altwin:prtsc_rwin

# xmodmap ~/.Xmodmap

# setxkbmap -layout us -option ctrl:nocaps,nodeadkeys,nbsp:level3,altwin:prtsc_rwin

#setxkbmap -layout us -option ctrl:nocaps_hyper_capscontrol,nodeadkeys,nbsp:level3,altwin:prtsc_rwin

# I am using orig, Mod3 is Hyper_L
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


# not sure what nodeadkeys does
# nodeadkeys
setxkbmap -layout us -option ctrl:hyper_capscontrol,nbsp:level3,altwin:prtsc_rwin

# /usr/share/X11/xkb/symbols/ctrl

xcape -e 'Control_L=Escape'

redshift -l 55.7:12.6 -t 2200:1600 -m randr &

unclutter &
dunst &

xrdb -merge ~/.Xresources


export SBCL_HOME="/usr/lib/sbcl/"

/home/benj/repos/stumpwm/stumpwm

