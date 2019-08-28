#!/usr/bin/env false

__rc='zsh'

if test -r "${HOME}/.profile"; then
	. "${HOME}/.profile"
fi

# zsh will source .zshrc on its own for interactive shells
