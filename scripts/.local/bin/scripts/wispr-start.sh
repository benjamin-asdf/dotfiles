#!/bin/sh

# Launches Wispr Flow, making sure picom is up first. Chromium/Electron
# decides once, at window-creation time, whether a compositor is present and
# renders transparent/overlay windows (like Wispr Flow's "Status" indicator)
# solid black if it isn't -- starting picom *after* Wispr Flow doesn't fix an
# already-open window, so the ordering here matters.

if ! systemctl --user is-active --quiet app-picom@autostart.service; then
	systemctl --user start app-picom@autostart.service
	sleep 0.5
fi

exec wispr-flow "$@"
