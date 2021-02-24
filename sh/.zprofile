#!/usr/bin/env false

if test -z "$__rc"; then
	__rc=zsh
fi

## shared profile

if test -r "$HOME/.profile"; then
	# shellcheck source=./.profile
	. "$HOME/.profile"
fi

## zsh will source .zshrc on its own for interactive login shells
