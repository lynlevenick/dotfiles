#!/usr/bin/env false

__pathadd() {
	# Adds a path to $PATH only if it isn't already present
	# Pass `prepend' as a second argument to prepend to path
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
