#!/usr/bin/env false

if test -t 1; then
    export CLICOLOR='1'
    export EDITOR='emacs -nw'
    export LSCOLORS='ExFxCxDxBxegedabagacad'
    export VISUAL='emacsclient -ua emacs'

    __attr_reset="$(tput sgr0)"

    __attr_red="$(tput setaf 1)"
    __attr_reverse="$(tput rev)"

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

    HISTCONTROL='ignoredups:erasedups'
    if test "$(command -v shopt)" = "shopt"; then
        shopt -s histappend
    fi

    if tput u7 >/dev/null; then
        # Terminal supports output of cursor position
        __read_cursor_position="$(tput u7)"
        __mark_unterminated() {
            IFS='[;' read -p "${__read_cursor_position}" -s -dR __unused __cursor_row __cursor_col
            if test "0${__cursor_col}" -gt 1; then
                printf '%s\n' "${__attr_reset}${__attr_reverse}%"
            fi
        }

        PROMPT_COMMAND="__mark_unterminated;${PROMPT_COMMAND}"
    fi

    if test -d "${HOME}/.config/sh-interactive"; then
        for __cmd in "${HOME}/.config/sh-interactive"/*; do
            . "${__cmd}"
        done
    fi
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
