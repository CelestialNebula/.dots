PATH="$PATH":"$HOME"/.scripts:"$HOME"/.local/bin

# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
export XDG_CONFIG_HOME="$HOME"/.config
export XDG_CACHE_HOME="$HOME"/.cache
export XDG_DATA_HOME="$HOME"/.local/share

export ENV="$XDG_CONFIG_HOME"/ksh/.kshrc
export VISUAL='vi'
export MANPAGER='less -i'
export LESSHISTFILE=/dev/null

export PYTHONSTARTUP="$XDG_CONFIG_HOME"/python/pythonstartup

# Autostart X at login
if [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ]; then
	exec startx;
fi
