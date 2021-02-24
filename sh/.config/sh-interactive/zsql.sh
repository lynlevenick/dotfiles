#!/usr/bin/env false

if ! command -v z >/dev/null; then
	printf 'z: fatal: not available\n' >&2
	return 1
fi

eval "$(z -S)"
