#!/usr/bin/env false

if test -z "$__rc"; then
	__rc=bash
fi

## shared profile

if test -r "$HOME/.profile"; then
	. "$HOME/.profile"
fi

## bashrc if necessary

if test -t 1; then
	if test -r "$HOME/.bashrc"; then
		. "$HOME/.bashrc"
	fi
fi
