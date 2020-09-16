#!/bin/sh
xsetroot -solid gray
. $HOME/.profile
xrdb $HOME/.Xresources
export ENV=$HOME/.config/ksh/.kshrc
xmodmap $HOME/.xmodmaprc
xidle &
exec cwm
