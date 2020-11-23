# /usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:
PATH=$PATH:$HOME/.scripts:$HOME/.local/bin

export ENV=$HOME/.config/ksh/.kshrc
export VISUAL='nvi'
# I don't see why I would want less to save history?
export LESSHISTFILE=/dev/null
export PYTHONSTARTUP=$HOME/.config/python/.pythonstartup

umask 077

# Autostart X at login
if [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ]; then
    exec startx;
fi
