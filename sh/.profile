#!/usr/bin/env false

# When dash is run as a login and interactive shell, it sources
# .profile, then it sources the file named by ENV. If dash is
# only run interactively, it only sources the file named by ENV.
# Ensure that we only source this file once
if test -n "${__profile_run}"; then
	return
fi
__profile_run=1

if test -z "${__rc}"; then
	__rc='unknown'
fi

# ENV determines non-login shell init file
ENV="${HOME}/.profile"; export ENV

__pathadd() {
	# Adds a path to $PATH only if it isn't already present
	# Pass `prepend` as a second argument to prepend to path
	case ":${PATH:=$1}:" in
		*:$1:*) ;;
		*)
			if test "$2" = 'prepend'; then
				PATH="$1:${PATH}"
			else
				PATH="${PATH}:$1"
			fi
	esac
}

if test -d "${HOME}/.config/sh"; then
	for __cmd in "${HOME}/.config/sh"/*; do
	. "${__cmd}"
	done
fi

# General-purpose run commands we'll always source since nothing
# gets clever and does so on its own.

if test -t 1 && test -r "${HOME}/.shrc"; then
	. "${HOME}/.shrc"
fi
