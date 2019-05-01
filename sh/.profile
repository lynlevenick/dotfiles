#!/usr/bin/env false

if test -t 1; then
    export CLICOLOR='1'
    export EDITOR='emacs -nw'
    export LSCOLORS='ExFxCxDxBxegedabagacad'
    export VISUAL='emacsclient -ua emacs'

    __ps1_err_color="$(tput setaf 1)"
    __ps1_err() {
        # shellcheck disable=SC2181
        if test "$?" -ne 0; then
            printf '%s' "${__ps1_err_color}"
        fi
    }
    __reset_color="$(tput sgr0)"
    case "${TERM}" in
        eterm*)
            PS1="\[${__reset_color}\]\w \[\$(__ps1_err)\]\\$\[${__reset_color}\] " ;;
        *)
            PS1="\[${__reset_color}\$(__ps1_err)\]\\$\[${__reset_color}\] "
    esac

    if test "$(command -v shopt)" = "shopt"; then
        HISTCONTROL='ignoredups:erasedups'
        shopt -s histappend
    fi

    for __cmd in "${HOME}/.config/sh-interactive"/*; do
        . "${__cmd}"
    done
fi

__pathadd() {
    # Adds a path to $PATH only if it isn't already present
    case ":${PATH:=$1}:" in
        *:$1:*) ;;
        *) PATH="${PATH}:$1"
    esac
}

if test -d "${HOME}/.cargo"; then
    export CARGO_HOME="${HOME}/.cargo"
    __pathadd "${CARGO_HOME}/bin"
fi

if test -r "${HOME}/.nvm/nvm.sh"; then
    export NVM_DIR="${HOME}/.nvm"
    . "${NVM_DIR}/nvm.sh"
fi

if test -d "${HOME}/.rustup"; then
    export RUSTUP_HOME="${HOME}/.rustup"
fi

if test -r "${HOME}/.rvm/scripts/rvm"; then
    . "${HOME}/.rvm/scripts/rvm"
    __pathadd "${HOME}/.rvm/bin"
fi
