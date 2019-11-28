#!/usr/bin/env false

if test -z "$__rc"; then
	__rc=unknown
fi

# ENV determines non-login shell init file
ENV="$HOME/.shrc"; export ENV

if test -z "$__rc_common_init"; then
	if test -d "$HOME/.config/sh-lib"; then
		for __cmd in "$HOME/.config/sh-lib"/*; do
			. "$__cmd"
		done
	fi
fi

if test -d "$HOME/.config/sh"; then
	for __cmd in "$HOME/.config/sh"/*; do
		. "$__cmd"
	done
fi

if test -z "$__rc_common_init"; then
	__rc_common_init=1

	if test -d "$HOME/.config/sh-common"; then
		for __cmd in "$HOME/.config/sh-common"/*; do
			. "$__cmd"
		done
	fi
fi

if test -t 1 && test "$__rc" = 'unknown'; then
	if test -r "$HOME/.shrc"; then
		. "$HOME/.shrc"
	fi
fi
