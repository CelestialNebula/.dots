HISTCONTROL=ignoredups:ignorespace
HISTFILE=$HOME/.config/ksh/.ksh_history

set -o emacs

export PYTHONSTARTUP=$HOME/.config/python/.pythonstartup

# Custom prompt
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
_PS1_GREEN="\[\e[32m\]"
_PS1_CLEAR="\[\e[0m\]"
# https://github.com/qbit/ohmyksh
_COLOR_ON_ERROR='\[\e[$(($??31:39))m\]'
PS1="$_PS1_GREEN\w$_PS1_CLEAR\n[$_COLOR_ON_ERROR\$?$_PS1_CLEAR]\$ "

OHMYKSH_DIR=$HOME/.config/ksh
. ${OHMYKSH_DIR}/ohmy.ksh

load_completion man
load_completion git
load_completion dots

# Alias
# In general, it is a bad practice to rename a command which already exists.
# Instead, create a new command with a different name.
# https://forums.freebsd.org/threads/security-doas-cant-work-with-zsh-alias.61539/#post-354457
# "It would be a serious security risk if aliases were inherited from the user.
# Imagine what would happen if the vi alias, besides opening the editor, also
# resets the root password?"

alias vi='/usr/bin/nvi'

# Confirm before overwriting something
alias mvc='/usr/bin/mv --interactive --verbose'
alias cpc='/usr/bin/cp --interactive --preserve=all --verbose'
alias rmc='/usr/bin/rm -i --verbose'

# Custom
alias grep='/usr/bin/grep --color=auto'
alias topc='/usr/bin/top -d 1.5 -1'
alias lsc='/usr/bin/ls --all --color=always --classify --group-directories-first --human-readable -l -v'
alias findd='/usr/bin/find ./ -type d -iname'
alias findf='/usr/bin/find ./ -type f -iname'
alias ec='/usr/bin/emacsclient --alternate-editor="" --create-frame'
# https://github.com/tmux/tmux/issues/142#issuecomment-586116296
# "FYI, looks like Tmux 3.1 (currently pre-release) is adding this feature:
# 15d7e56."
alias tmux='/usr/bin/tmux -f $HOME/.config/tmux/tmux.conf'
alias dots='/usr/bin/git --git-dir=$HOME/.dots/ --work-tree=$HOME'
alias youtube-dlc='youtube-dlc --config-location $HOME/atlas/software/configuration/youtube-dlc '
alias youtube-dlcb='youtube-dlc --config-location $HOME/atlas/software/configuration/youtube-dlc --batch-file "$HOME/atlas/software/configuration/youtube-dlc/batch"'
alias youtube-dl='echo Use youtube-dlc'
alias wgetn='/usr/bin/wget --report-speed=bits --show-progress --wait=3 --random-wait --directory-prefix $HOME/atlas/unsorted/wget/ --adjust-extension --ignore-length --convert-links --page-requisites --no-parent'
alias wgetr='/usr/bin/wget --report-speed=bits --show-progress --wait=3 --random-wait --directory-prefix $HOME/atlas/unsorted/wget/ --adjust-extension --ignore-length --recursive --level=inf --convert-links --page-requisites --no-parent'
alias rsyncc='/usr/bin/rsync -vv --info=progress2,stats2 --archive --update --hard-links --itemize-changes -hhh'
alias mpvyt='/usr/bin/mpv --ytdl-format=22'
