PATH="$PATH":/usr/games:"$HOME"/.scripts:"$HOME"/.local/bin
export PATH HOME TERM

# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
export XDG_CONFIG_HOME="$HOME"/.config
export XDG_CACHE_HOME="$HOME"/.cache
export XDG_DATA_HOME="$HOME"/.local/share

export ENV="$XDG_CONFIG_HOME/ksh"/.kshrc
export VISUAL='vi'
export MANPAGER='less -i'

export PYTHONSTARTUP="$XDG_CONFIG_HOME"/python/pythonstartup

xrdb "$XDG_CONFIG_HOME"/Xresources
