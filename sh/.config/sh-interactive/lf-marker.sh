#!/usr/bin/env false

__attr_reverse="$(tput rev)"
__attr_unset="$(tput sgr0)"
__cli_reset_line="\015$(tput el)"

__lf_marker() {
	__cols="$(tput cols)"
	printf "$__attr_unset$__attr_reverse%%$__attr_unset%*s$__cli_reset_line" "$((__cols - 1))" ''
}

if test -z "$ZSH_VERSION"; then
	PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}__lf_marker"
fi
