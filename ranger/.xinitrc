source ~/.profile
# Fix java apps with tiling WMs
export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1
export GDK_CORE_DEVICE_EVENTS=1

# setroot -cursor_name left_ptr

xset r rate 140 220

# lock after 5 minutes
xautolock -time 5 -locker i3lock &

xrandr --auto

[ -f .screenlayout/layout.sh ] && .screenlayout/layout.sh


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

# for kinesis, I remapped esc to capslock then it worked

xcape -e 'Control_L=Escape'

redshift -l 55.7:12.6 -t 2200:1600 -m randr &
unclutter &
dunst &

xrdb -merge ~/.Xresources
xmodmap ~/.Xmodmap





export XMODIFIERS=@im=fcitx
export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx

fcitx5 &

# Tag the session as a plain X11 desktop. Without these, systemd sees
# XDG_SESSION_TYPE=tty (started from a console) and XDG_CURRENT_DESKTOP is
# empty, so portals and other graphical user services can't identify the env.
export XDG_SESSION_TYPE=x11
export XDG_CURRENT_DESKTOP=stumpwm

# Push X display + desktop vars into BOTH the D-Bus activation environment and
# the systemd --user manager (--systemd) so user services and D-Bus-activated
# portals (e.g. xdg-desktop-portal-gtk, which renders browser file pickers)
# can open a display instead of dying with "cannot open display:". This
# supersedes a plain `systemctl --user import-environment`.
dbus-update-activation-environment --systemd \
    DISPLAY XAUTHORITY XDG_SESSION_TYPE XDG_CURRENT_DESKTOP

# StumpWM isn't launched by a systemd session manager, so nothing pulls in
# graphical-session.target (it refuses manual start). This custom target
# Wants it, satisfying the Requisite= gate on xdg-desktop-portal.service so
# the GTK file-picker portal can activate. (Flameshot bypasses the portal via
# useX11LegacyScreenshot — there's no Screenshot backend for a bare X11 WM.)
systemctl --user start stumpwm-session.target

exec stumpwm


