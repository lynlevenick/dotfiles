export CLICOLOR='1'
export EDITOR='code --wait'

export PS1="\[$(tput sgr0)\]\\$ "

shopt -s histappend

export FZF_DEFAULT_OPTS='--preview-window up'
fedit() {
    if [ "$#" -gt 1 ]; then
        printf 'fatal: Too many arguments\n'
        return 1
    fi

    local dir="${1:-.}"
    if [ ! -d "${dir}" ]; then
        printf 'fatal: Not a directory: %s\n' "${dir}"
        return 1
    fi

    rg -0 --files -- "${dir}" | fzf --read0 --print0 -m --preview 'head -n${LINES} {}' | xargs -0 -I% -R1 code -g %
}
fkill() {
    ps -Arc -opid=,command= | awk '{ print $1, $2 }' | fzf -m --tiebreak=index | cut -d' ' -f1 | xargs kill "$@" --
}
