#!/usr/bin/env false

if command -v brew >/dev/null; then
	return 1
fi

if test -e "/opt/homebrew/bin/brew"; then
	eval "$(/opt/homebrew/bin/brew shellenv)"
fi
