if [ -t 1 ]; then
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
    PS1="\[${__reset_color}\$(__ps1_err)\]\\$\[${__reset_color}\] "

    alias please='sudo $(history -p !!)'

    if [ "$(command -v shopt)" = "shopt" ]; then
        HISTCONTROL='ignoredups:erasedups'
        shopt -s histappend
    fi
fi

__pathadd() {
    # Adds a path to $PATH only if it isn't already present
    case ":${PATH:=$1}:" in
        *:$1:*) ;;
        *) PATH="${PATH}:$1" ;;
    esac
}

if [ -d "${HOME}/.cargo" ]; then
    export CARGO_HOME="${HOME}/.cargo"
    __pathadd "${HOME}/.cargo/bin"
fi

if [ -r "${HOME}/.nvm/nvm.sh" ]; then
    export NVM_DIR="${HOME}/.nvm"
    . "${NVM_DIR}/nvm.sh"
fi

if [ -d "${HOME}/.rustup" ]; then
    export RUSTUP_HOME="${HOME}/.rustup"
fi

if [ -r "${HOME}/.rvm/scripts/rvm" ]; then
    . "${HOME}/.rvm/scripts/rvm"
    __pathadd "${HOME}/.rvm/bin"
fi
