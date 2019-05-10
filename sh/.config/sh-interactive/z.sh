#!/usr/bin/env false

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

    PROMPT_COMMAND="__zsql_add;${PROMPT_COMMAND}"
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

    if test "0${__zsql_sum}" -gt 5000; then
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
    if test "$1" = '$'; then
        printf "fatal: z: Not in history\n"
    elif test -n "${1%??}"; then
        cd "${1%??}" 2>/dev/null || printf "fatal: cd: %s not found\n" "${1%??}"
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
