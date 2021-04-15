#!/usr/bin/env false

case "$(uname)" in
	Darwin)
	    XDG_CACHE_HOME="$(getconf DARWIN_USER_CACHE_DIR | sed 's/\/*$//')"; export XDG_CACHE_HOME ;;
	*)
esac

__cache_home="${XDG_CACHE_HOME:-$HOME/.cache}"
__config_home="${XDG_CONFIG_HOME:-$HOME/.config}"

if test -z "$__rc"; then
	__rc=unknown
fi

# ENV determines non-login shell init file
__rc_env_was_set="${ENV:+yes}"
ENV="$HOME/.shrc"; export ENV

if test -z "$__rc_common_init"; then
	__rc_common_init=1

	if test -d "$__config_home/sh-lib"; then
		for __cmd in "$__config_home/sh-lib"/*; do
			# shellcheck source=/dev/null
			. "$__cmd"
		done
	fi

	if test -d "$__config_home/sh"; then
		for __cmd in "$__config_home/sh"/*; do
			# shellcheck source=/dev/null
			. "$__cmd"
		done
	fi

	if test -d "$__config_home/sh-common"; then
		for __cmd in "$__config_home/sh-common"/*; do
			# shellcheck source=/dev/null
			. "$__cmd"
		done
	fi
fi

if test "$__rc_env_was_set" != yes && test -t 1 && test "$__rc" = unknown; then
	if test -r "$HOME/.shrc"; then
		# shellcheck source=./.shrc
		. "$HOME/.shrc"
	fi
fi
