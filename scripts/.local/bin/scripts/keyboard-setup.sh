#!/bin/sh

# All keyboard state of an X session in one idempotent place: autorepeat,
# layout + xkb options, the Escape-on-tap daemon, and the keycode fixups.
# Called from ~/.xinitrc at session start, and by hand whenever something
# resets the keymap -- replugging the Kinesis, an xkb-touching app, a
# `setxkbmap` experiment gone wrong.

# The first number is the delay before autorepeat starts, the second the
# repeat rate. Deliberately different from xrate-fast (120 250), which is
# the post-unlock setting in lock-screen.sh.
xset r rate 140 220

# ctrl:hyper_capscontrol comes from a custom symbols block (see below):
# CapsLock becomes Control_L, left Control becomes Hyper_L on Mod3.
# altwin:prtsc_rwin puts Print on the right Super key.
#
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
#
# not sure what nodeadkeys does
# nodeadkeys
#
# The bare `-option ""` is load-bearing: -option *appends* to whatever the
# server already has, so without the reset a second run leaves the list
# duplicated (...,ctrl:hyper_capscontrol,nbsp:level3,... twice over).
setxkbmap -layout us \
          -option "" \
          -option ctrl:hyper_capscontrol,nbsp:level3,altwin:prtsc_rwin

# keycode 112 = Insert (and the commented-out Hyper_R modifier experiment)
xmodmap ~/.Xmodmap

# Tap-left-Control (physically CapsLock, keycode 66, which hyper_capscontrol
# turned into Control_L) for Escape.
# for kinesis, I remapped esc to capslock then it worked
#
# LAST, and always restarted: xcape resolves its keysyms to keycodes once at
# startup and then watches them via XRecord, so any later setxkbmap/xmodmap
# leaves the running daemon watching a keycode that no longer means what it
# thought -- it stays alive and simply stops emitting Escape. That makes
# "leave the existing one alone" the wrong guard here: this script's whole
# job is to have just reset the keymap. Kill and respawn instead, which also
# keeps taps from doubling if an old instance were still live.
pkill -x xcape
xcape -e 'Control_L=Escape'
