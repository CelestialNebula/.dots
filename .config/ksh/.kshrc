set -o noclobber -o emacs

HISTCONTROL=ignoredups:ignorespace
HISTFILE="$XDG_CONFIG_HOME"/ksh/ksh_history
HISTSIZE=8000

# ^Delete
bind '^[[3;5~'=delete-word-forward

# https://codereview.stackexchange.com/questions/174019/ps1-for-bash-prompt-showing-last-exit-status-written-mostly-in-python/176291#176291
custom_PS1() {
	local TITLE
	local EXITSTATUS

	# https://invisible-island.net/xterm/xterm.faq.html#how2_title
	TITLE='\033]2;\u@\h: \w\007'
	EXITSTATUS='${?#0}'
	# Only set TITLE for the terminals that support it
	case "$TERM" in
	xterm*)
		PS1="\[$TITLE\]\w\n$EXITSTATUS\$ " ;;
	*)
		PS1="\w\n$EXITSTATUS\$ " ;;
	esac
        unset -f custom_PS1
}
custom_PS1

# https://github.com/qbit/ohmyksh
OHMYKSH_DIR="$XDG_CONFIG_HOME"/ksh
if [ -r "$OHMYKSH_DIR"/ohmy.ksh ]; then
	. "${OHMYKSH_DIR}"/ohmy.ksh
	load_completion man
	load_completion rc
	load_completion ssh
	load_completion echo
	load_completion git
	load_completion dots
else
	echo 'Missing "ohmy.ksh"!'
fi

# https://seb.jambor.dev/posts/improving-shell-workflows-with-fzf/#virtual-env
pyvirt() {
       local selected_env

       selected_env=$(find "$HOME"/src/venv/ -maxdepth 1 ! -path "$HOME"/src/venv/ -type d -exec basename {} ';' | fzf)

       if [ -n "$selected_env" ]; then
               . "$HOME"/src/venv/"$selected_env"/bin/activate
       fi
}

if [ -r /usr/local/share/ksh/fzf_stuff.ksh ]; then
	. /usr/local/share/ksh/fzf_stuff.ksh
	alias ff='fuzzyfindfile'
else
	echo "Missing extra ~fzf~ stuff."
fi

# Says it's a bad idea to have '--preview' in FZF_DEFAULT_OPTS, see if anything breaks
# https://github.com/junegunn/fzf#preview-window
export FZF_DEFAULT_OPTS='--preview "cat -v {}"
--preview-window=sharp:hidden
--bind "ctrl-e:execute(xdg-open {}),ctrl-t:toggle-preview,ctrl-i:execute(less -i {})"'

alias rebootc='doas reboot'
alias shutdownc='doas shutdown -p now'
alias pgadd='doas pkg_add -v'
alias pgdelete='doas pkg_delete -v'
alias pginfo='pkg_info'
alias ls='ls -AFhl'
alias ca='cat -v'
alias top='top -CS -o cpu -s 1.5'
# https://github.com/tmux/tmux/commit/15d7e564ddab575dd3ac803989cc99ac13b57198
# "...(in Makefile.am, so portable tmux only)."
alias tmux='tmux -f "$XDG_CONFIG_HOME"/tmux.conf'
alias ec='emacsclient -ca ""'
alias ez='emacs -Q'
alias dots='git --git-dir="$HOME"/.dots/ --work-tree="$HOME"'
