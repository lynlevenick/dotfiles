#!/usr/bin/env false

if command -v sqlite3 >/dev/null; then
	__zsql_cache="${XDG_CACHE_HOME:-$HOME/.cache}/zsql_cache.db"

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

__zsql_cd() {
	CDPATH='' cd -- "${1%??}" 2>/dev/null || printf 'fatal: cd: %s not found\n' "${1%??}"
}

__zsql_forget() {
	while :; do
		printf 'Remove '\''%s'\''? [Yn] ' "${1%??}"
		read -r __zsql_yn
		case "${__zsql_yn}" in
			''|y|Y)
				break ;;
			n|N)
				return ;;
			*)
				printf 'Please respond Y or N.\n'
		esac
	done

	__zsql_escaped="$(printf '%s$' "${1%??}" | sed 's/'\''/'\'\''/g')"
	sqlite3 "${__zsql_cache}" <<SQL
.timeout 100
DELETE FROM dirs WHERE dir = '${__zsql_escaped%?}';
SQL
}

z() {
	__zsql_action='__zsql_cd'
	unset __zsql_case_sensitive

	while :; do
		case "$1" in
			--case-sensitive)
				__zsql_case_sensitive=1 ;;
			-f|--forget)
				__zsql_action='__zsql_forget' ;;
			--)
				shift
				break ;;
			*)
				break ;;
		esac

		shift
	done

	if test -z "$__zsql_case_sensitive" && printf '%s' "$*" | grep -q -E '[A-Z]'; then
		__zsql_case_sensitive=1
	fi

	__zsql_escaped_pwd="$(printf '%s$' "$(pwd)" | sed 's/'\''/'\'\''/g')"
	if test -n "$*"; then
		__zsql_filtered_search="$(printf '%s$' "$*" | sed -e 's/\(.\)/%\1/g' -e 's/'\''/'\'\''/g')"
		__zsql_selection="$(
			sqlite3 "${__zsql_cache}" <<SQL | xargs printf '%b\0' | fzf --read0 --print0 --filter="$*" | rg --text --multiline --only-matching --max-count=1 '(?-u)^([^\x00]+)' && printf '$'
.mode tcl
.timeout 100
${__zsql_case_sensitive:+PRAGMA case_sensitive_like = ON;}
SELECT dir FROM dirs
	WHERE dir != '${__zsql_escaped_pwd%?}'
	AND dir LIKE '${__zsql_filtered_search%?}'
	ORDER BY frecency DESC;
SQL
		)"
	else
		__zsql_selection="$(
			sqlite3 "${__zsql_cache}" <<SQL | xargs printf '%b\0' | fzf-tmux --read0 --select-1 --bind='?:toggle-preview' --preview='env CLICOLOR_FORCE=1 ls -G -- {}' --preview-window=hidden && printf '$'
.mode tcl
.timeout 100
SELECT dir FROM dirs ORDER BY frecency DESC;
SQL
		)"
	fi

	if test "${__zsql_selection}" = '$'; then
		printf 'fatal: z: Not in history\n'
	elif test -n "${__zsql_selection%??}"; then
		"${__zsql_action}" "${__zsql_selection}"
	fi
}
