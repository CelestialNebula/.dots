#. /etc/ksh.kshrc

set -o emacs

HISTCONTROL=ignoredups:ignorespace
HISTFILE=$XDG_CONFIG_HOME/ksh/.ksh_history
HISTSIZE=4000

# Custom prompt
# https://invisible-island.net/xterm/xterm.faq.html#how2_title
_TITLE='\033]2;\u@\h: \w\007'
# https://misc.flogisoft.com/bash/tip_colors_and_formatting
_PS1_GREEN='\[\e[32m\]'
_PS1_CLEAR='\[\e[0m\]'
_PS1_COLOR_ON_ERROR='\[\e[$(($??31:39))m\]'
PS1="$_TITLE$_PS1_GREEN\w$_PS1_CLEAR\n[$_PS1_COLOR_ON_ERROR\$?$_PS1_CLEAR]\$ "

# https://github.com/qbit/ohmyksh
OHMYKSH_DIR=$XDG_CONFIG_HOME/ksh
if [ -f "$OHMYKSH_DIR"/ohmy.ksh ]; then
	. "${OHMYKSH_DIR}"/ohmy.ksh
	load_completion man
	load_completion ssh
	load_completion echo
	load_completion git
	load_completion dots
	load_completion systemctl
else
	builtin echo "Missing 'ohmy.ksh'!"
fi

# ^Delete
bind '^[[3;5~'=delete-word-forward

# ^o = Add a way I can cd into a directory when I'm just looking ie. fzf, I see
# .kshrc (under ~/.config/ksh) I hit ^o and I'm in ~/.config/ksh
export FZF_DEFAULT_COMMAND='/usr/bin/find ./ -type f'
# (xdg-open {})+abort will drop to the base command line when I exit whatever I opened.
# https://github.com/junegunn/fzf/issues/1907#issuecomment-593996676
# "...fzf reads from '/dev/tty' instead of stdin"
export FZF_DEFAULT_OPTS='--preview "cat {}" --preview-window=sharp:hidden --bind "ctrl-e:execute(xdg-open {}),ctrl-t:toggle-preview,ctrl-i:execute(less {} > /dev/tty)"'

fzf_history() {
	local FZFHISTORY
	FZFHISTORY="$(fc -ln 1 | fzf --tac | sed -e 's/^[[:space:]]//')"
	builtin print -s "$FZFHISTORY"
	# This seems sub optimal, cause it runs after every call and it would
	# only remove 1 after the first time.  But I don't know how to have
	# 'fzf_history' replaced by what the command was?
	/usr/bin/sed -i.back -e '/^fzf_history$/d' "$XDG_CONFIG_HOME"/ksh/.ksh_history
}
bind -m '^R'='^Ufzf_history^J^P^J'

# fcd {/EXAMPLE/} - Will show directory's (recursively) under the current
# directory or /EXAMPLE/
fcd() {
	local DIR
	# Look under "Parameters" for the explanation for "${1:-.}".
	# If the first command line argument is unset, replace it with '.'
	# This is what allows you to just do ~fcd~ or ~fcd /EXAMPLE/~
	DIR=$(find "${1:-.}" -type d 2> /dev/null | fzf) && cd "$DIR" || return
}

fuzzyfindfile() {
	find "${1:-.}" -type f 2> /dev/null | fzf || return
}

alias ls='ls --almost-all --escape --color=always --group-directories-first --human-readable -l'
alias grep='/usr/bin/grep --color=auto'
alias top='/usr/bin/top -d 1.5 -1'
alias ec='/usr/bin/emacsclient --alternate-editor="" --create-frame'
alias dots='/usr/bin/git --git-dir=$HOME/.dots/ --work-tree=$HOME'
alias ff='fuzzyfindfile'
alias yt-dl='youtube-dlc --config-location $HOME/atlas/software/configuration/youtube-dlc'
alias yt-dlb='youtube-dlc --config-location $HOME/atlas/software/configuration/youtube-dlc --batch-file "$HOME/atlas/software/configuration/youtube-dlc/batch"'
alias rsyncc='/usr/bin/rsync -vv --info=progress2,stats2 --archive --update --hard-links --itemize-changes -hhh'
alias wgetn='/usr/bin/wget --execute robots=off --report-speed=bits --show-progress --wait=3 --random-wait --directory-prefix $HOME/atlas/unsorted/wget/ --adjust-extension --ignore-length --convert-links --page-requisites --no-parent'
alias wgetr='/usr/bin/wget --execute robots=off --report-speed=bits --show-progress --wait=3 --random-wait --directory-prefix $HOME/atlas/unsorted/wget/ --adjust-extension --ignore-length --recursive --level=inf --convert-links --page-requisites --no-parent'
alias mpvyt='/usr/bin/mpv --ytdl-format=22'
# For when it's really hard
alias mpvyth='/usr/bin/mpv --ytdl-format=18'
alias mpvyth1='/usr/bin/mpv --ytdl-format=300'
