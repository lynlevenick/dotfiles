#!/usr/bin/env false

if command -v sqlite3 >/dev/null; then
	__zsql_cache="${XDG_CACHE_HOME:-$HOME/.cache}/zsql_cache.db"

	if test ! -r "$__zsql_cache"; then
		mkdir -p "$(dirname "$__zsql_cache")"

		sqlite3 "$__zsql_cache" <<SQL >/dev/null
CREATE TABLE dirs (dir TEXT NOT NULL, frecency INTEGER NOT NULL DEFAULT 1);
CREATE UNIQUE INDEX index_by_dir ON dirs (dir);
CREATE INDEX index_by_frecency_and_dir ON dirs (frecency, dir);
CREATE TRIGGER trigger_on_update_forget
	AFTER UPDATE OF frecency ON dirs
	WHEN (SELECT SUM(frecency) FROM dirs) >= 5000
	BEGIN
	UPDATE dirs SET frecency = CAST(frecency * 0.9 AS INTEGER);
	DELETE FROM dirs WHERE frecency = 0;
	END;
SQL
	fi

	PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}__zsql_add_async"
fi

# Escape a string for sqlite, by turning single quotes into two single quotes
__zsql_escape() {
	sed 's/'\''/'\'\''/g'
}

__zsql_read_to_nul_octal() {
	od -v -b | sed -n ':loop
s/^[^ ]*//
s/ *$//
/ 000/b end
s/  */ \\/g
p
n
b loop
:end
s/ *000.*//
s/  */ \\/g
p
q'
}

# Consume standard input, printing until the first NUL byte
if test -n "$ZSH_VERSION"; then
	__zsql_read_to_nul() {
		# ZSH's builtin printf doesn't support octal escapes,
		# use the system one and explicitly word-split the output

		# shellcheck disable=2034
		__zsql_octal="$(__zsql_read_to_nul_octal)"
		# shellcheck disable=SC2086
		command printf '%b' ${=__zsql_octal}
	}
else
	__zsql_read_to_nul() {
		# shellcheck disable=SC2046
		printf '%b' $(__zsql_read_to_nul_octal)
	}
fi

__zsql_add() {
	__zsql_escaped="$(printf '%s$' "$1" | __zsql_escape)"

	sqlite3 "$__zsql_cache" <<SQL
.timeout 100
INSERT INTO dirs (dir)
	VALUES ('${__zsql_escaped%?}')
	ON CONFLICT (dir) DO UPDATE SET
	frecency = frecency + excluded.frecency;
SQL
}
__zsql_add_async() {
	(__zsql_add "$(pwd)" &)
}

__zsql_cd() {
	CDPATH='' cd -- "${1%?}" 2>/dev/null || printf 'fatal: cd: `%s'\'' not found\n' "${1%?}"
}

__zsql_forget() {
	while :; do
		printf 'Remove `%s'\''? [Yn] ' "${1%?}"
		IFS='' read -r __zsql_yn
		case "$__zsql_yn" in
			''|y|Y)
				break ;;
			n|N)
				return ;;
			*)
				printf 'Please respond Y or N.\n'
		esac
	done

	__zsql_escaped="$(printf '%s$' "${1%?}" | __zsql_escape)"
	sqlite3 "$__zsql_cache" <<SQL
.timeout 100
DELETE FROM dirs WHERE dir = '${__zsql_escaped%?}';
SQL
}

z() {
	__zsql_action=__zsql_cd
	unset __zsql_case_sensitive

	while :; do
		case "$1" in
			--case-sensitive)
				__zsql_case_sensitive=1 ;;
			-f|--forget)
				__zsql_action=__zsql_forget ;;
			--)
				shift
				break ;;
			*)
				break
		esac

		shift
	done

	if test -z "$__zsql_case_sensitive" && printf '%s' "$*" | grep -q -E '[A-Z]'; then
		__zsql_case_sensitive=1
	fi

	if test -n "$*"; then
		__zsql_escaped_pwd="$(printf '%s$' "$(pwd)" | __zsql_escape)"
		__zsql_filtered_search="$(printf '%s$' "$*" | sed -e 's/\(.\)/%\1/g' -e 's/'\''/'\'\''/g')"
		__zsql_selection="$(
			sqlite3 "$__zsql_cache" <<SQL | xargs printf '%b\0' 2>/dev/null | fzf --read0 --print0 --filter="$*" | __zsql_read_to_nul && printf '$'
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
			sqlite3 "$__zsql_cache" <<SQL | xargs printf '%b\0' 2>/dev/null | fzf-tmux --read0 --select-1 --bind='?:toggle-preview' --preview='env CLICOLOR_FORCE=1 ls -G -- {}' --preview-window=hidden && printf '$'
.mode tcl
.timeout 100
SELECT dir FROM dirs ORDER BY frecency DESC;
SQL
		)"
	fi

	__zsql_exitstatus="$?"
	if test "0$__zsql_exitstatus" -eq 130; then
		return 0 # User probably manually exited with C-C, C-D, or C-G
	elif test -z "$__zsql_selection" || test "$__zsql_selection" = '$'; then
		printf 'fatal: z: Not in history\n'
		return 1
	elif test -n "${__zsql_selection%?}"; then
		"$__zsql_action" "$__zsql_selection"
	fi
}
