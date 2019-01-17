if [ -n "${TERM}" ]; then
    export CLICOLOR='1'
    export EDITOR='emacs -nw'
    export LSCOLORS='ExFxCxDxBxegedabagacad'
    export VISUAL='emacs'

    __ps1_err_color="$(tput setaf 1)"
    __ps1_err() {
        if [ "$?" -ne 0 ]; then
            printf '%s' "${__ps1_err_color}"
        fi
    }
    __reset_color="$(tput sgr0)"
    export PS1="\[${__reset_color}\$(__ps1_err)\]\\$\[${__reset_color}\] "

    alias please='sudo $(history -p !!)'

    HISTCONTROL='ignoredups:erasedups'
    shopt -s histappend
fi

if [ -d "${HOME}/.cargo" ]; then
    export CARGO_HOME="${HOME}/.cargo"

    if [ -d "${HOME}/.rustup" ]; then
        export RUSTUP_HOME="${HOME}/.rustup"
    fi

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
