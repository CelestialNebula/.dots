# /usr/local/sbin:/usr/local/bin:/usr/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl:
PATH=$PATH:"$HOME/.scripts:$HOME/.local/bin"

# XDG stuff
# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

export ENV="$XDG_CONFIG_HOME/ksh/.kshrc"
export VISUAL='vi'
# I don't see why I would want less to save history?
export LESSHISTFILE=/dev/null
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/.pythonstartup"

/usr/bin/xrdb "$XDG_CONFIG_HOME/.Xresources"

# Autostart X at login
if [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ]; then
	exec startx;
fi
