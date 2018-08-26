export CLICOLOR="1"
export EDITOR="emacs -nw"
export VISUAL="emacs"

__ps1_err_color="$(tput setaf 1)"
__ps1_err() {
    if [ "$?" -ne 0 ]; then
        printf '%s' "${__ps1_err_color}"
    fi
}
export PS1="\[$(tput sgr0)\$(__ps1_err)\]\\$\[$(tput sgr0)\] "

shopt -s histappend
