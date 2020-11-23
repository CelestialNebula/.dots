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
load_completion git
load_completion dots

# Alias
# In general, it is a bad practice to rename a command which already exists.
# Instead, create a new command with a different name.
# https://forums.freebsd.org/threads/security-doas-cant-work-with-zsh-alias.61539/#post-354457
# "It would be a serious security risk if aliases were inherited from the user.
# Imagine what would happen if the vi alias, besides opening the editor, also
# resets the root password?"

alias shutdownc='doas shutdown -p now'
alias lsc='/bin/ls -AFhl'
alias topc='/usr/bin/top -C -o cpu -S -s 1.5'
# https://github.com/tmux/tmux/issues/142#issuecomment-586116296
# "FYI, looks like Tmux 3.1 (currently pre-release) is adding this feature:
# 15d7e56."
# It doesn't appear that OpenBSD is adding this
alias tmux='/usr/bin/tmux -f $HOME/.config/tmux/tmux.conf'
alias dots='/usr/local/bin/git --git-dir=$HOME/.dots/ --work-tree=$HOME'
