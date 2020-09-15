# /usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:
PATH=$PATH:$HOME/.scripts:$HOME/.local/bin

umask 077

export ENV=$HOME/.config/ksh/.kshrc
export VISUAL='nvi'
# I don't see why I would want less to save history?
export LESSHISTFILE=/dev/null

# Autostart X at login
if [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ]; then
    exec startx;
fi
