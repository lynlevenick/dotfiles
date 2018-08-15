export CLICOLOR=1
export EDITOR='code --wait'

export PS1="\[$(tput sgr0)\]\\$ "

bind 'set mark-symlinked-directories on'
shopt -s histappend

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

export FZF_DEFAULT_OPTS="--preview-window up"
search() {
    rg --vimgrep "${@:-\A}" | fzf -d: -n4..,.. -m --preview 'tail -n"+"{2} {1} | head -n${LINES}' | cut -d: -f1-3 | xargs -I% -R1 code -g %
}
edit() {
    if [ "$#" -gt 1 ]; then
        echo 'Cannot provide more than 1 argument'
        return 1
    fi

    local dir="${1:-.}"
    if [ ! -d "${dir}" ]; then
        echo 'Must provide a directory'
        return 1
    fi

    rg --files "${dir}" | fzf -m --preview 'head -n${LINES} {}' | xargs -I% -R1 code -g %
}
