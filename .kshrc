HISTCONTROL=ignoredups:ignorespace
HISTFILE=$HOME/.ksh_history

set -o emacs

# Custom prompt
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
_PS1_BROWN="\[\033[38;5;166m\]"
_PS1_CLEAR="\[\033[0m\]"
_PS1_GREEN="\[\033[32m\]"
PS1="$_PS1_BROWN\u@\h$_PS1_CLEAR:$_PS1_GREEN\w$_PS1_CLEAR\n\$ "

# Alias
# In general, it is a bad practice to rename a command which already exists.
# Instead, create a new command with a different name.
# https://forums.freebsd.org/threads/security-doas-cant-work-with-zsh-alias.61539/#post-354457
# "It would be a serious security risk if aliases were inherited from the user.
# Imagine what would happen if the vi alias, besides opening the editor, also
# resets the root password?"

# Confirm before overwriting something
alias mvc='/bin/mv -iv'
alias cpc='/bin/cp -ipv'
alias rmc='/bin/rm -iv'

# Custom
alias topc='/usr/bin/top -C -o cpu -S -s 1.5'
alias lsc='/bin/ls -aFhl'
alias catc='/bin/cat -nv'
# https://github.com/tmux/tmux/issues/142#issuecomment-586116296
# "FYI, looks like Tmux 3.1 (currently pre-release) is adding this feature:
# 15d7e56."
# It doesn't appear that OpenBSD is adding this tho?
alias tmux='/usr/bin/tmux -f $HOME/.config/tmux/tmux.conf'
alias dots='/usr/local/bin/git --git-dir=$HOME/.dots/ --work-tree=$HOME'
