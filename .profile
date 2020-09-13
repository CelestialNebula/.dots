# /usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:
PATH=$PATH:$HOME/.scripts:$HOME/.local/bin

export VISUAL="nvi"

# The environment global umask is set in /etc/profile
# This doesn't seem to take effect?
umask 077

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# Autostart X at login
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec startx
fi
