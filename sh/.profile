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
            PS1="\[${__reset_color}\]\w \[$(__ps1_err)\]\\$\[${__reset_color}\] " ;;
        *)
            PS1="\[${__reset_color}\$(__ps1_err)\]\\$\[${__reset_color}\] "
    esac

    if test "$(command -v shopt)" = "shopt"; then
        HISTCONTROL='ignoredups:erasedups'
        shopt -s histappend
    fi

    __ef_action() {
        if test -n "${1%??}"; then
            ${VISUAL:-${EDITOR:-vi}} -- "${1%??}"
        fi
    }

    if command -v highlight >/dev/null; then
        __ef_highlighter='(highlight --out-format=ansi {} 2>/dev/null || cat {})'
    else
        __ef_highlighter='cat {}'
    fi

    ef() {
        __ef_action "$(
            (rg --files --hidden --null 2>/dev/null |
             fzf-tmux --read0 --select-1 --query="$*" \
                      --bind='?:toggle-preview' \
                      --preview='
                          case "$(file --mime {})" in
                              *binary*)
                                  printf '\''%s: binary file'\'' {} ;;
                              *)
                                  '"${__ef_highlighter}"' |
                                  head -n "$((LINES * 4))"
                          esac' \
                      --preview-window=hidden
            ) && printf '$'
        )"
    }

    if command -v sqlite3 >/dev/null; then
        __zsql_cache="${HOME}/.cache/zsql_cache.db"

        if test ! -r "${__zsql_cache}"; then
            mkdir -p "$(dirname "${__zsql_cache}")"

            sqlite3 "${__zsql_cache}" <<SQL >/dev/null
PRAGMA journal_mode=WAL;

CREATE TABLE dirs (dir TEXT, frecency INTEGER);
CREATE UNIQUE INDEX index_by_dir ON dirs (dir);
CREATE INDEX index_by_frecency_and_dir ON dirs (frecency, dir);
SQL
        fi

        PROMPT_COMMAND="__zsql_add; ${PROMPT_COMMAND}"
    fi

    __zsql_add_async() {
        __zsql_escaped="$(printf '%s$' "${1}" | sed 's/'\''/'\'\''/g')"
        sqlite3 "${__zsql_cache}" <<SQL
.timeout 100
INSERT INTO dirs (dir, frecency)
    VALUES ('${__zsql_escaped%?}', 1)
    ON CONFLICT (dir) DO UPDATE SET
    frecency = frecency + excluded.frecency;
SQL
        __zsql_sum="$(
            sqlite3 "${__zsql_cache}" <<SQL
.timeout 100
SELECT SUM(frecency) FROM dirs;
SQL
        )"

        if test "0${__zsql_sum}" -gt 1000; then
            sqlite3 "${__zsql_cache}" <<SQL
.timeout 100
BEGIN TRANSACTION;
UPDATE dirs SET frecency = CAST(frecency * 0.9 AS INTEGER);
DELETE FROM dirs WHERE frecency <= 0;
COMMIT TRANSACTION;
SQL
        fi
    }
    __zsql_add() {
        (__zsql_add_async "${PWD}" &)
    }

    __zsql_action() {
        if test -n "${1%??}"; then
            cd "${1%??}" || printf "fatal: cd: %s" "${1%??}"
        fi
    }

    z() {
        if test -n "$*"; then
            __zsql_action "$(
                sqlite3 "${__zsql_cache}" <<SQL | xargs printf '%s\0' | fzf-tmux --read0 --filter="$*" | head -n1 && printf '$'
.mode tcl
.timeout 100
SELECT dir FROM dirs ORDER BY frecency DESC;
SQL
            )"
        else
            __zsql_action "$(
                sqlite3 "${__zsql_cache}" <<SQL | xargs printf '%s\0' | fzf-tmux --read0 --select-1 --bind='?:toggle-preview' --preview='env CLICOLOR_FORCE=1 ls -G -- {}' --preview-window=hidden && printf '$'
.mode tcl
.timeout 100
SELECT dir FROM dirs ORDER BY frecency DESC;
SQL
            )"
        fi
    }
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
