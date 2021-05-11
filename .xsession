#!/bin/sh

xsetroot -solid gray &
. "$HOME"/.profile
xidle &
if [ -r "$XDG_CONFIG_HOME"/xmodmaprc ]; then
	xmodmap "$XDG_CONFIG_HOME"/xmodmaprc
fi
emacs --daemon &

exec cwm -c "$XDG_CONFIG_HOME"/cwmrc
