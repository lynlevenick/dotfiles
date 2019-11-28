#!/usr/bin/env false

# regexp-escape $HOME
__dynamic_title_home_pattern="$(printf '%s' "$HOME" | sed 's/[]\/$.^|[]/\\&/g')"

__dynamic_title() {
	printf '\033]0;%s\007' "$(printf '%s' "$PWD" | sed "s/^$__dynamic_title_home_pattern/~/")"
}

case "$TERM" in
	xterm*|alacritty)
		PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}__dynamic_title" ;;
	*)
esac
