#!/usr/bin/env false

__rc='bash'

if test -r "${HOME}/.bashrc"; then
	. "${HOME}/.bashrc"
elif test -r "${HOME}/.profile"; then
	. "${HOME}/.profile"
fi
