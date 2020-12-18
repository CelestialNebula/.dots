#!/usr/bin/env sh
if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for file in /etc/X11/xinit/xinitrc.d/?*.sh; do
		[ -x "$file" ] && . "$file"
	done
	unset file
fi

/usr/bin/xsetroot -solid gray &
/usr/bin/pipewire &
/usr/bin/dunst &
# /usr/bin/emacs --daemon &

exec /usr/bin/cwm -c "$XDG_CONFIG_HOME/.cwmrc"
