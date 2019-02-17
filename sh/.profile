#!/usr/bin/env false

if test -t 1; then
    export CLICOLOR='1'
    export EDITOR='emacs -nw'
    export LSCOLORS='ExFxCxDxBxegedabagacad'
    export VISUAL='emacs'

    __ps1_err_color="$(tput setaf 1)"
    __ps1_err() {
        # shellcheck disable=SC2181
        if test "$?" -ne 0; then
            printf '%s' "${__ps1_err_color}"
        fi
    }
    __reset_color="$(tput sgr0)"
    PS1="\[${__reset_color}\$(__ps1_err)\]\\$\[${__reset_color}\] "

    if test "$(command -v shopt)" = "shopt"; then
        HISTCONTROL='ignoredups:erasedups'
        shopt -s histappend
    fi

    __ef_action() {
        if test -n "${__files}"; then
            printf "%s" "${__files}" | xargs -0 "${VISUAL:-${EDITOR:-vi}}"
        fi
    }
    __ef_files() {
        rg --files --hidden -0 "$@" 2>/dev/null
    }
    __ef_fzf() {
        fzf-tmux --read0 --print0 --exit-0 --select-1 --multi "$@"
    }

    ef() {
        __files="$(__ef_files | __ef_fzf --query="$*")"
        __ef_action
    }
    efp() {
        __files="$(__ef_files | __ef_fzf --query="$*" --preview='case "$(file --mime {})" in *binary*) echo {}: binary file ;; *) cat {} || head -n "${LINES}" ;; esac')"
        __ef_action
    }
fi

__pathadd() {
    # Adds a path to $PATH only if it isn't already present
    case ":${PATH:=$1}:" in
        *:$1:*) ;;
        *) PATH="${PATH}:$1" ;;
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
