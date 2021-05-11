#!/usr/bin/env sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for file in /etc/X11/xinit/xinitrc.d/?*.sh; do
		[ -x "$file" ] && . "$file"
	done
	unset file
fi

xrdb "$XDG_CONFIG_HOME"/Xresources

xsetroot -solid gray &
pipewire &
picom &
emacs --daemon &

exec cwm -c "$XDG_CONFIG_HOME"/cwmrc
