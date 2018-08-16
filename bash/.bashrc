export CLICOLOR='1'
export EDITOR='code --wait'

__ps1_err_color="$(tput setaf 1)"
__ps1_err() {
    if [ "$?" -ne 0 ]; then
        printf '%s' "${__ps1_err_color}"
    fi
}
export PS1="\[$(tput sgr0)\$(__ps1_err)\]\\$\[$(tput sgr0)\] "

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

    rg -0 --files -- "${dir}" | fzf --read0 --print0 -m --preview 'head -n${LINES} {}' | xargs -0 -n1 -- code -g
}
fkill() {
    ps -Arc -opid=,command= | awk '{for(i=1;i<=NF;++i)printf"%s ",$i;print""}' | fzf -m --tiebreak=index --preview 'ps -c -o%cpu,%mem -p {1..}' --preview-window up:2 | cut -d' ' -f1- | xargs kill "$@" --
}
