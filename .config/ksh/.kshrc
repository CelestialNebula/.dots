if [ -r /etc/ksh.kshrc ]; then
	. /etc/ksh.kshrc
else
	set -o emacs
fi

HISTCONTROL=ignoredups:ignorespace
HISTFILE=$HOME/.config/ksh/ksh_history
HISTSIZE=4000

# https://invisible-island.net/xterm/xterm.faq.html#how2_title
_TITLE='\[\033]2;\u@\h: \w\007\]'
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
_PS1_GREEN='\[\033[32m\]'
_PS1_CLEAR='\[\033[0m\]'
_COLOR_ON_ERROR='\[\033[$(($??31:39))m\]'
PS1="\[$_TITLE$_PS1_GREEN\w$_PS1_CLEAR\n[$_COLOR_ON_ERROR\$?$_PS1_CLEAR]\$\] "

set -o noclobber

# https://github.com/qbit/ohmyksh
OHMYKSH_DIR=$HOME/.config/ksh
if [ -f "$OHMYKSH_DIR"/ohmy.ksh ]; then
	. "${OHMYKSH_DIR}"/ohmy.ksh
	load_completion man
	load_completion ssh
	load_completion echo
	load_completion git
	load_completion dots
else
	echo "Missing 'ohmy.ksh'"
fi

# ^Delete
bind '^[[3;5~'=delete-word-forward

if [ -r /usr/local/share/ksh/fzf_stuff.ksh ]; then
	. /usr/local/share/ksh/fzf_stuff.ksh
else
	echo "Missing ~fzf~ stuff."
fi

export FZF_DEFAULT_COMMAND='find "${1:-./}" -type f'
# Says it's a bad idea to have '--preview' in FZF_DEFAULT_OPTS, see if anything breaks
export FZF_DEFAULT_OPTS='--preview "cat {}" --preview-window=sharp:hidden --bind "ctrl-e:execute(xdg-open {}),ctrl-t:toggle-preview,ctrl-i:execute(less -i {})"'

alias shutdownc='doas shutdown -p now'
alias pgadd='doas pkg_add -v'
alias pgdelete='doas pkg_delete -v'
alias pginfo='pkg_info'
alias ls='ls -AFhl'
alias top='top -CS -o cpu -s 1.5'
# https://github.com/tmux/tmux/commit/15d7e564ddab575dd3ac803989cc99ac13b57198
# "...(in Makefile.am, so portable tmux only)."
alias tmux='tmux -f "$XDG_CONFIG_HOME"/tmux.conf'
alias dots='git --git-dir="$HOME"/.dots/ --work-tree="$HOME"'
alias ff='fzf'
