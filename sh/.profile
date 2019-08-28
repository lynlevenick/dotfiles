#!/usr/bin/env false

__rc="${__rc:-unknown}"

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
