#!/usr/bin/env false

if test -t 1; then
    export CLICOLOR='1'
    export EDITOR='emacs -nw'
    export LSCOLORS='ExFxCxDxBxegedabagacad'
    export VISUAL='emacs -nw'

    if test -n "$BASH_VERSION"; then
        __attr_red="$(tput setaf 1)"
        __attr_reset="$(tput sgr0)"

        __ps1_err() {
            # shellcheck disable=SC2181
            if test "$?" -ne 0; then
                printf '%s' "${__attr_red}"
            fi
        }
        case "${TERM}" in
            eterm*)
                PS1="\[${__attr_reset}\]\w \[\$(__ps1_err)\]\\$\[${__attr_reset}\] " ;;
            *)
                PS1="\[${__attr_reset}\$(__ps1_err)\]\\$\[${__attr_reset}\] "
        esac
    fi

    HISTCONTROL='ignoredups:erasedups'
    if test "$(command -v shopt)" = "shopt"; then
        shopt -s histappend
    fi

    if test -d "${HOME}/.config/sh-interactive"; then
        for __cmd in "${HOME}/.config/sh-interactive"/*; do
            . "${__cmd}"
        done
    fi
fi

__pathadd() {
    # Adds a path to $PATH only if it isn't already present
    # Pass `prepend` as a second argument to prepend to path
    case ":${PATH:=$1}:" in
        *:$1:*) ;;
        *)
            if test "$2" = 'prepend'; then
                PATH="$1:${PATH}"
            else
                PATH="${PATH}:$1"
            fi
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
