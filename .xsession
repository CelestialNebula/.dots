#!/usr/bin/env sh
xsetroot -solid gray
. "$HOME"/.profile
if [ -f "$HOME"/.xmodmaprc ]; then
	/usr/X11R6/bin/xmodmap "$HOME"/.xmodmaprc
fi
/usr/X11R6/bin/xidle &
exec /usr/X11R6/bin/cwm -c "$XDG_CONFIG_HOME/.cwmrc"
