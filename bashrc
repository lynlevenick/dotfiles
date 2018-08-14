export CLICOLOR=1
export EDITOR="$(command -v code) --wait"

reset_colors=$(tput sgr0)
export PS1="\[${reset_colors}\]\\$ "

bind 'set mark-symlinked-directories on'
shopt -s histappend

edit() {
    if [ -z "$@" ]; then
        echo 'Must provide a search'
        return 1
    fi
    rg --vimgrep "$@" | fzf -d: -m --preview 'tail -n"+"{2} {1} | head -n${LINES}' | cut -d: -f1-3 | xargs -I% -R1 code -g %
}
