if [ -r /etc/ksh.kshrc ]; then
	. /etc/ksh.kshrc
else
	set -o emacs
fi

HISTCONTROL=ignoredups:ignorespace
HISTFILE=$XDG_CONFIG_HOME/ksh/ksh_history
HISTSIZE=4000

# https://invisible-island.net/xterm/xterm.faq.html#how2_title
_TITLE='\[\033]2;\u@\h: \w\007\]'
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
_PS1_GREEN='\[\033[32m\]'
_PS1_CLEAR='\[\033[0m\]'
_PS1_COLOR_ON_ERROR='\[\033[$(($??31:39))m\]'
PS1="\[$_TITLE$_PS1_GREEN\w$_PS1_CLEAR\n[$_PS1_COLOR_ON_ERROR\$?$_PS1_CLEAR]\$\] "

set -o noclobber

# https://github.com/qbit/ohmyksh
OHMYKSH_DIR=$XDG_CONFIG_HOME/ksh
if [ -f "$OHMYKSH_DIR"/ohmy.ksh ]; then
	. "${OHMYKSH_DIR}"/ohmy.ksh
	load_completion man
	load_completion ssh
	load_completion echo
	load_completion git
	load_completion dots
	load_completion pacman
	load_completion systemctl
else
	echo "Missing 'ohmy.ksh'!"
fi

# ^Delete
bind '^[[3;5~'=delete-word-forward

if [ -r /usr/share/ksh/fzf_stuff.ksh ]; then
	. /usr/share/ksh/fzf_stuff.ksh
else
	echo "Missing ~fzf~ stuff."
fi

export FZF_DEFAULT_COMMAND='find "${1:-./}" -type f'
# Says it's a bad idea to have '--preview' in FZF_DEFAULT_OPTS, see if anything breaks
export FZF_DEFAULT_OPTS='--preview "cat {}" --preview-window=sharp:hidden --bind "ctrl-e:execute(xdg-open {}),ctrl-t:toggle-preview,ctrl-i:execute(less -i {})"'

alias ls='ls -AFhlv'
alias top='top -1'
alias ec='emacsclient -ca ""'
alias dots='git --git-dir="$HOME"/.dots/ --work-tree="$HOME"'
alias ff='fzf'
alias ytd='youtube-dlc'
alias ytdb='youtube-dlc --batch-file "$XDG_CONFIG_HOME"/yt-dlp/batch'
alias rsyncc='rsync -vvauHihhh --info=progress2,stats2'
alias wgetn='wget -kpnp -e robots=off --report-speed=bits --show-progress -w 3 --random-wait -P "$HOME"/atlas/unsorted/wget/ -E --ignore-length --hsts-file="$XDG_CACHE_HOME"/wget-hsts'
alias wgetr='wget -rkpl inf -np -e robots=off --report-speed=bits --show-progress -w 3 --random-wait -P "$HOME"/atlas/unsorted/wget/ -E --ignore-length --hsts-file="$XDG_CACHE_HOME"/wget-hsts'
alias mpvyt='mpv --ytdl-format=22'
alias mpvyth='mpv --ytdl-format=18'
alias mpvyth1='mpv --ytdl-format=300'
