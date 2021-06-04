#!/usr/bin/env false

__dynamic_title() {
	printf '\033]0;%s\007' "$(__ps1_pwd)"
}

case "$TERM" in
	xterm*|alacritty)
		PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}__dynamic_title" ;;
	*)
esac
