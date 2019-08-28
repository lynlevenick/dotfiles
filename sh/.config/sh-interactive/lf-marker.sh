#!/usr/bin/env false

__attr_reverse="$(tput rev)"
__attr_reset="$(tput sgr0)"
__cli_reset_line="\015$(tput el)"

__lf_marker() {
	__cols="$(tput cols)"
	printf "${__attr_reverse}%%${__attr_reset}%*s${__cli_reset_line}" "$((__cols - 1))" ''
}

case "${__rc}" in
	fish|zsh) ;;
	*)
		PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}__lf_marker"
esac
