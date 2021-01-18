. /etc/ksh.kshrc

HISTCONTROL=ignoredups:ignorespace
HISTFILE=$HOME/.config/ksh/.ksh_history
HISTSIZE=4000

# https://invisible-island.net/xterm/xterm.faq.html#how2_title
_TITLE='\[\033]2;\u@\h: \w\007\]'
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
_PS1_GREEN='\[\033[32m\]'
_PS1_CLEAR='\[\033[0m\]'
_COLOR_ON_ERROR='\[\033[$(($??31:39))m\]'
PS1="\[$_TITLE$_PS1_GREEN\w$_PS1_CLEAR\n[$_COLOR_ON_ERROR\$?$_PS1_CLEAR]\$\] "

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
	builtin echo "Missing 'ohmy.ksh'"
fi

# ^Delete
bind '^[[3;5~'=delete-word-forward

# ^o = Add a way I can cd into a directory when I'm just looking ie. fzf, I see
# .kshrc (under ~/.config/ksh) I hit ^o and I'm in ~/.config/ksh
export FZF_DEFAULT_COMMAND='find ./ -type f'
# (xdg-open {})+abort will drop to the base command line when I exit whatever I opened.
# https://github.com/junegunn/fzf/issues/1907#issuecomment-593996676
# "...fzf reads from '/dev/tty' instead of stdin"
export FZF_DEFAULT_OPTS='--preview "cat {}" --preview-window=sharp:hidden --bind "ctrl-e:execute(xdg-open {}),ctrl-t:toggle-preview,ctrl-i:execute(less -i {})"'

fzf_history() {
	local FZFHISTORY
	FZFHISTORY="$(fc -ln 1 | fzf --tac | sed -e 's/^[[:space:]]//')"
	print -s "$FZFHISTORY"
	# This seems sub optimal, cause it runs after every call and it would
	# only remove 1 after the first time.  But I don't know how to have
	# 'fzf_history' replaced by what the command was?
	sed -i.back -e '/^fzf_history$/d' "$XDG_CONFIG_HOME"/ksh/.ksh_history
}
bind -m '^R'='^Ufzf_history^J^P^J'

# fcd {/EXAMPLE/} - Will show directory's (recursively) under the current
# directory or /EXAMPLE/
fcd() {
	local DIR
	# Look under "Parameters" for the explanation for "${1:-.}".
	# This is what allows you to just do ~fcd~ or ~fcd /EXAMPLE/~
	DIR=$(find "${1:-.}" -type d 2> /dev/null | fzf) && cd "$DIR" || return
}

fuzzyfindfile() {
	find "${1:-.}" -type f 2> /dev/null | fzf || return
}

alias shutdownc='doas shutdown -p now'
alias ls='ls -AFhl'
alias top='top -CS -o cpu -s 1.5'
# https://github.com/tmux/tmux/commit/15d7e564ddab575dd3ac803989cc99ac13b57198
# "...(in Makefile.am, so portable tmux only)."
alias tmux='tmux -f $XDG_CONFIG_HOME/tmux/tmux.conf'
alias dots='git --git-dir=$HOME/.dots/ --work-tree=$HOME'
alias ff='fuzzyfindfile'
