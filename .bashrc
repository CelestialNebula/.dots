# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

shopt -s histappend
HISTCONTROL=ignorespace:ignoredups

# Custom prompt
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
_PS1_GREEN='\[\e[32m\]'
_PS1_CLEAR='\[\e[0m\]'
# https://github.com/qbit/ohmyksh
_COLOR_ON_ERROR='\[\e[$(($??31:39))m\]'
PS1="$_PS1_GREEN\w$_PS1_CLEAR\n[$_COLOR_ON_ERROR\$?$_PS1_CLEAR]\$ "

# Custom title
# If this is an xterm set the title to: user@host:dir
# This needs to be after you set your current PS1 prompt.
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;\u@\h: \w\a\]$PS1"
        ;;
    *)
        ;;
esac

# Bash completion (bash-completion package is installed by default I think)
# https://www.howtoforge.com/how-to-add-bash-completion-in-debian
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Alias
# In general, it's a bad practice to rename a command which already exists.
# Instead, create a new command with a different name.
# https://forums.freebsd.org/threads/security-doas-cant-work-with-zsh-alias.61539/#post-354457
# "It would be a serious security risk if aliases were inherited from the user.
# Imagine what would happen if the vi alias, besides opening the editor, also
# resets the root password?"

# Custom
alias grepc='/usr/bin/grep --color=auto'
alias topc='/usr/bin/top -d 1.5 -1'
alias lsc='/usr/bin/ls --almost-all --color=always --group-directories-first --human-readable -l'
alias findd='/usr/bin/find ./ -type d -iname'
alias findf='/usr/bin/find ./ -type f -iname'
alias ec='/usr/bin/emacsclient --alternate-editor="" --create-frame'
alias dots='/usr/bin/git --git-dir=$HOME/.dots/ --work-tree=$HOME'
alias yt-dlc='youtube-dlc --config-location $HOME/atlas/software/configuration/youtube-dlc '
alias yt-dlcb='youtube-dlc --config-location $HOME/atlas/software/configuration/youtube-dlc --batch-file "$HOME/atlas/software/configuration/youtube-dlc/batch"'
alias wgetn='/usr/bin/wget --report-speed=bits --show-progress --wait=3 --random-wait --directory-prefix $HOME/atlas/unsorted/wget/ --adjust-extension --ignore-length --convert-links --page-requisites --no-parent'
alias wgetr='/usr/bin/wget --report-speed=bits --show-progress --wait=3 --random-wait --directory-prefix $HOME/atlas/unsorted/wget/ --adjust-extension --ignore-length --recursive --level=inf --convert-links --page-requisites --no-parent'
alias rsyncc='/usr/bin/rsync -vv --info=progress2,stats2 --archive --update --hard-links --itemize-changes -hhh'
alias mpvyt='/usr/bin/mpv --ytdl-format=22'

# If running interactively, exit
# https://stackoverflow.com/questions/40747576/need-help-understanding-a-strange-bashrc-expression/40750440#40750440
case $- in
    *i*)
    ;;
    *)
        return;;
esac
