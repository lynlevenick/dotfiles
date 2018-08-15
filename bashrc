export CLICOLOR='1'
export EDITOR='code --wait'

export PS1="\[$(tput sgr0)\]\\$ "

shopt -s histappend

export FZF_DEFAULT_OPTS='--preview-window up'
search() {
    rg --vimgrep -- "${@:-\A}" | fzf -d: -n4..,.. -m --preview 'tail -n+{2} {1} | head -n${LINES}' | cut -d: -f1-3 | xargs -I% -R1 code -g %
}
edit() {
    if [ "$#" -gt 1 ]; then
        printf 'fatal: Too many arguments\n'
        return 1
    fi

    local dir="${1:-.}"
    if [ ! -d "${dir}" ]; then
        printf 'fatal: Not a directory: %s\n' "${dir}"
        return 1
    fi

    rg --files -- "${dir}" | fzf -m --preview 'head -n${LINES} {}' | xargs -I% -R1 code -g %
}
