#!/usr/bin/env false

__cache_home="${XDG_CACHE_HOME:-$HOME/.cache}"
__config_home="${XDG_CONFIG_HOME:-$HOME/.config}"

if test -z "$__rc"; then
	__rc=unknown
fi

# ENV determines non-login shell init file
ENV="$HOME/.shrc"; export ENV

if test -z "$__rc_common_init"; then
	if test -d "$__config_home/sh-lib"; then
		for __cmd in "$__config_home/sh-lib"/*; do
			. "$__cmd"
		done
	fi
fi

if test -d "$__config_home/sh"; then
	for __cmd in "$__config_home/sh"/*; do
		. "$__cmd"
	done
fi

if test -z "$__rc_common_init"; then
	__rc_common_init=1

	if test -d "$__config_home/sh-common"; then
		for __cmd in "$__config_home/sh-common"/*; do
			. "$__cmd"
		done
	fi
fi

if test -t 1 && test "$__rc" = 'unknown'; then
	if test -r "$HOME/.shrc"; then
		. "$HOME/.shrc"
	fi
fi
