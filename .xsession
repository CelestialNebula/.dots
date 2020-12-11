/usr/bin/env sh
xsetroot -solid gray
if [ -f "$HOME"/.profile ]; then
	. "$HOME"/.profile
fi
if [ -f "$HOME"/.Xresources ]; then
	xrdb "$HOME"/.Xresources
fi
if [ -f "$HOME"/.config/ksh/.kshrc ]; then
	builtin export ENV=$HOME/.config/ksh/.kshrc
fi
if [ -f "$HOME"/.xmodmaprc ]; then
	xmodmap "$HOME"/.xmodmaprc
fi
xidle &
exec cwm
