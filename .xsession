#!/usr/bin/env sh
/usr/X11R6/bin/xsetroot -solid gray &
. "$HOME"/.profile
if [ -f "$HOME"/.xmodmaprc ]; then
	/usr/X11R6/bin/xmodmap "$HOME"/.xmodmaprc
fi
/usr/X11R6/bin/xidle &
/usr/local/bin/emacs --daemon &

exec /usr/X11R6/bin/cwm -c "$XDG_CONFIG_HOME/cwmrc"
