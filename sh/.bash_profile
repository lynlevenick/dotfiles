#!/usr/bin/env false

__rc='bash'

if test -r "${HOME}/.profile"; then
	. "${HOME}/.profile"
fi

# bash will not source bashrc for login shells - if input is
# a terminal, we're probably interactive so source .bashrc

if test -t 1; then
	if test -r "${HOME}/.bashrc"; then
		. "${HOME}/.bashrc"
	fi
fi
