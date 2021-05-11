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
	load_completion ssh
	load_completion echo
	load_completion git
	load_completion dots
	load_completion pacman
	load_completion systemctl
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

if [ -r /usr/share/ksh/fzf_stuff.ksh ]; then
	. /usr/share/ksh/fzf_stuff.ksh
	alias ff='fuzzyfindfile'
else
	echo 'Missing extra ~fzf~ stuff.'
fi

# Says it's a bad idea to have '--preview' in FZF_DEFAULT_OPTS, see if anything breaks
# https://github.com/junegunn/fzf#preview-window
export FZF_DEFAULT_OPTS='--preview "cat -v {}"
--preview-window=sharp:hidden
--bind "ctrl-e:execute(xdg-open {}),ctrl-t:toggle-preview,ctrl-i:execute(less -i {})"'

alias l='ls -AFhlv'
alias ca='cat -v'
alias ec='emacsclient -ca ""'
alias ez='emacs -Q'
alias dots='git --git-dir="$HOME"/.dots/ --work-tree="$HOME"'
alias yd='yt-dlp'
alias ydb='yt-dlp --batch-file "$XDG_CONFIG_HOME"/yt-dlp/batch'
alias rsyncc='rsync -vvauHihhh --info=progress2,stats2'
alias wgetn='wget -kpnp -e robots=off --report-speed=bits --show-progress -w 3 --random-wait -P "$HOME"/atlas/unsorted/wget/ -E --ignore-length --hsts-file="$XDG_CACHE_HOME"/wget-hsts'
alias wgetr='wget -rkpl inf -np -e robots=off --report-speed=bits --show-progress -w 3 --random-wait -P "$HOME"/atlas/unsorted/wget/ -E --ignore-length --hsts-file="$XDG_CACHE_HOME"/wget-hsts'
alias mpvnv='mpv --vid=no --sub=no'
alias mpvyt='mpv --ytdl-format=22'
alias mpvyth='mpv --ytdl-format=18'
alias mpvyth1='mpv --ytdl-format=300'
