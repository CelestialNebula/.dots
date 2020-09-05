xsetroot -solid gray
. $HOME/.profile
xrdb $HOME/.Xresources
export ENV=$HOME/.kshrc
xmodmap $HOME/.xmodmaprc
xidle &
exec cwm
