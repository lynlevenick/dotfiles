export CLICOLOR="1"
export EDITOR="emacs -nw"
export VISUAL="emacs"

if [ -n "${TERM}" ]; then
    __ps1_err_color="$(tput setaf 1)"
    __ps1_err() {
        if [ "$?" -ne 0 ]; then
            printf '%s' "${__ps1_err_color}"
        fi
    }
    export PS1="\[$(tput sgr0)\$(__ps1_err)\]\\$\[$(tput sgr0)\] "
fi

shopt -s histappend

if [ -d "${HOME}/.cargo" ]; then
    export PATH="${PATH}:${HOME}/.cargo/bin"
fi

if [ -r "${HOME}/.nvm/nvm.sh" ]; then
    export NVM_DIR="${HOME}/.nvm"
    source "${NVM_DIR}/nvm.sh"
fi

if [ -r "${HOME}/.rvm/scripts/rvm" ]; then
    source "${HOME}/.rvm/scripts/rvm"
    export PATH="${PATH}:${HOME}/.rvm/bin"
fi
