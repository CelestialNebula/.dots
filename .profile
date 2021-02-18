# /bin:/usr/bin:/sbin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin
PATH=$PATH:/usr/games:"$HOME/.scripts:$HOME/.local/bin"
export PATH HOME TERM

# XDG stuff
# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

export ENV="$XDG_CONFIG_HOME/ksh/.kshrc"
export VISUAL='vi'
export MANPAGER='less -i'
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonstartup"

/usr/X11R6/bin/xrdb "$XDG_CONFIG_HOME/Xresources"
