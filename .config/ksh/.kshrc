. /etc/ksh.kshrc

HISTCONTROL=ignoredups:ignorespace
HISTFILE=$HOME/.config/ksh/.ksh_history
HISTSIZE=4000

set -o emacs

# Custom prompt
# https://invisible-island.net/xterm/xterm.faq.html#how2_title
_TITLE='\033]2;\u@\h: \w\007'
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
_PS1_GREEN='\[\e[32m\]'
_PS1_CLEAR='\[\e[0m\]'
_COLOR_ON_ERROR='\[\e[$(($??31:39))m\]'
PS1="$_TITLE$_PS1_GREEN\w$_PS1_CLEAR\n[$_COLOR_ON_ERROR\$?$_PS1_CLEAR]\$ "

# ^Delete
bind '^[[3;5~'=delete-word-forward

# https://github.com/qbit/ohmyksh
OHMYKSH_DIR=$HOME/.config/ksh
. ${OHMYKSH_DIR}/ohmy.ksh

load_completion man
load_completion echo
load_completion ssh
load_completion git
load_completion dots
load_completion dnf

# Alias
# In general, it is a bad practice to rename a command which already exists.
# Instead, create a new command with a different name.
# https://forums.freebsd.org/threads/security-doas-cant-work-with-zsh-alias.61539/#post-354457
# "It would be a serious security risk if aliases were inherited from the user.
# Imagine what would happen if the vi alias, besides opening the editor, also
# resets the root password?"

alias vi='/usr/bin/nvi'

alias lsc='/usr/bin/ls --almost-all --color=always --group-directories-first --human-readable -l'
alias grepc='/usr/bin/grep --color=auto'
alias findd='/usr/bin/find ./ -type d -iname'
alias findf='/usr/bin/find ./ -type f -iname'
alias topc='/usr/bin/top -d 1.5 -1'
alias ec='/usr/bin/emacsclient --alternate-editor="" --create-frame'
alias yt-dlc='youtube-dlc --config-location $HOME/atlas/software/configuration/youtube-dlc'
alias yt-dlcb='youtube-dlc --config-location $HOME/atlas/software/configuration/youtube-dlc --batch-file "$HOME/atlas/software/configuration/youtube-dlc/batch"'
alias dots='/usr/bin/git --git-dir=$HOME/.dots/ --work-tree=$HOME'
alias wgetn='/usr/bin/wget --report-speed=bits --show-progress --wait=3 --random-wait --directory-prefix $HOME/atlas/unsorted/wget/ --adjust-extension --ignore-length --convert-links --page-requisites --no-parent'
alias wgetr='/usr/bin/wget --report-speed=bits --show-progress --wait=3 --random-wait --directory-prefix $HOME/atlas/unsorted/wget/ --adjust-extension --ignore-length --recursive --level=inf --convert-links --page-requisites --no-parent'
alias rsyncc='/usr/bin/rsync -vv --info=progress2,stats2 --archive --update --hard-links --itemize-changes -hhh'
alias mpvyt='/usr/bin/mpv --ytdl-format=22'
