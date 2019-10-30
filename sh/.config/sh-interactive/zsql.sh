#!/usr/bin/env false

if command -v sqlite3 >/dev/null; then
	__zsql_cache="${HOME}/.cache/zsql_cache.db"

	if test ! -r "${__zsql_cache}"; then
		mkdir -p "$(dirname "${__zsql_cache}")"

		sqlite3 "${__zsql_cache}" <<SQL >/dev/null
CREATE TABLE dirs (dir TEXT NOT NULL, frecency INTEGER NOT NULL);
CREATE UNIQUE INDEX index_by_dir ON dirs (dir);
CREATE INDEX index_by_frecency_and_dir ON dirs (frecency, dir);
SQL
	fi

	PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}__zsql_add_async"
fi

__zsql_add() {
	__zsql_escaped="$(printf '%s$' "${1}" | sed 's/'\''/'\'\''/g')"
	__zsql_sum="$(
		sqlite3 "${__zsql_cache}" <<SQL
.timeout 100
INSERT INTO dirs (dir, frecency)
	VALUES ('${__zsql_escaped%?}', 1)
	ON CONFLICT (dir) DO UPDATE SET
	frecency = frecency + excluded.frecency;
SELECT SUM(frecency) FROM dirs;
SQL
	)"

	if test "0${__zsql_sum}" -gt 5000; then
		sqlite3 "${__zsql_cache}" <<SQL
.timeout 100
BEGIN;
UPDATE dirs SET frecency = CAST(frecency * 0.9 AS INTEGER);
DELETE FROM dirs WHERE frecency <= 0;
COMMIT;
SQL
	fi
}
__zsql_add_async() {
	(__zsql_add "$(pwd)" &)
}

__zsql_action() {
	if test "$1" = '$'; then
		printf 'fatal: z: Not in history\n'
	elif test -n "${1%??}"; then
		CDPATH= cd -- "${1%??}" 2>/dev/null || printf 'fatal: cd: %s not found\n' "${1%??}"
	fi
}

z() {
	__zsql_escaped_pwd="$(printf '%s$' "$(pwd)" | sed 's/'\''/'\'\''/g')"
	if test -n "$*"; then
		__zsql_filtered_search="$(printf '%s$' "$*" | sed -e 's/\(.\)/%\1/g' -e 's/'\''/'\'\''/g')"
		__zsql_action "$(
			sqlite3 "${__zsql_cache}" <<SQL | xargs printf '%b\0' | fzf --read0 --print0 --filter="$*" | rg --text --multiline --only-matching --max-count=1 '(?-u)^([^\x00]+)' && printf '$'
.mode tcl
.timeout 100
SELECT dir FROM dirs
	WHERE dir != '${__zsql_escaped_pwd%?}'
	AND dir LIKE '${__zsql_filtered_search%?}'
	ORDER BY frecency DESC;
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
