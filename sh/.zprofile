#!/usr/bin/env false

__rc='zsh'

if test -r "${HOME}/.zshrc"; then
	. "${HOME}/.zshrc"
elif test -r "${HOME}/.profile"; then
	. "${HOME}/.profile"
fi
