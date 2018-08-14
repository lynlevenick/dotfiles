export CLICOLOR=1
export EDITOR="$(command -v code) --wait"

reset_colors=$(tput sgr0)
export PS1="\[${reset_colors}\]\\$ "

bind 'set mark-symlinked-directories on'
shopt -s histappend

search() {
    if [ "$#" -lt 1 ]; then
        echo 'Must provide a search'
        return 1
    fi
    rg --vimgrep "$@" | fzf -d: -m --preview 'tail -n"+"{2} {1} | head -n${LINES}' --preview-window=up | cut -d: -f1-3 | xargs -I% -R1 code -g %
}
edit() {
    local dir
    if [ "$#" -gt 1 ]; then
        echo 'Cannot provide a search'
        return 1
    elif [ "$#" -eq 1 ]; then
        dir="$1"
    else
        dir="."
    fi
    rg --files "${dir}" | fzf -m --preview 'head -n${LINES} {}' --preview-window=up | xargs -I% -R1 code -g %
}
