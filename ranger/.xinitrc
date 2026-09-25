source ~/.profile

# Take this session's cgroup off systemd-oomd's shortlist. Xorg and StumpWM run
# in session-N.scope, and oomd's swap rule considers every descendant cgroup and
# kills whichever holds the most swap -- on 2026-09-03 that was this scope, at
# 6.8G, so the entire desktop died to reclaim memory a JVM had allocated. The
# helper sets user.oomd_avoid on the scope, making it a candidate of last resort.
# `|| true`: a missing sudoers entry must never block the session from starting.
sudo -n /usr/local/sbin/oomd-avoid-session || true

# Fix java apps with tiling WMs
export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1
export GDK_CORE_DEVICE_EVENTS=1

# setroot -cursor_name left_ptr

# The idle locker now lives in xautolock.service (pulled in by
# stumpwm-session.target), not here. Backgrounding it from .xinitrc left it
# unsupervised: on 2026-09-04 the instance started here died and lingered as a
# zombie child of StumpWM, while a hand-started replacement -- orphaned to
# PID 1, cwd ~/repos/game, DISPLAY=:0.0 -- was what actually locked the screen.
# systemd gives it one owner, a restart policy, and a predictable environment.

xrandr --auto

[ -f .screenlayout/layout.sh ] && .screenlayout/layout.sh


# Autorepeat, layout + xkb options, xcape and ~/.Xmodmap all live in one
# idempotent script now, so the keymap can be restored mid-session (after
# replugging the Kinesis, say) without picking lines out of this file.
keyboard-setup.sh

redshift -l 55.7:12.6 -t 2200:1600 -m randr &
unclutter &
dunst &

xrdb -merge ~/.Xresources





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

# KDE Connect (phone integration). In a full Plasma session the daemon
# autostarts via /etc/xdg/autostart/org.kde.kdeconnect.daemon.desktop, but
# StumpWM doesn't process XDG autostart files, so launch it by hand. Started
# after the dbus activation env is set so it picks up DISPLAY + the session bus.
kdeconnectd &

# StumpWM isn't launched by a systemd session manager, so nothing pulls in
# graphical-session.target (it refuses manual start). This custom target
# Wants it, satisfying the Requisite= gate on xdg-desktop-portal.service so
# the GTK file-picker portal can activate. (Flameshot bypasses the portal via
# useX11LegacyScreenshot — there's no Screenshot backend for a bare X11 WM.)
systemctl --user start stumpwm-session.target

exec stumpwm


