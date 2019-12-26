#!/usr/bin/env false

if test -r "$HOME/.rvm/scripts/rvm"; then
	if test -t 1; then
		__ruby_commands='bundle gem irb ri ruby rvm'

		# Unnecessary printf makes this work in zsh and other shells too
		# shellcheck disable=SC2086
		for __command in $(printf '%s\n' $__ruby_commands); do
			# shellcheck disable=SC2139
			alias $__command="unalias $__ruby_commands && . \"\$HOME/.rvm/scripts/rvm\" && $__command"
		done
	else
		. "$HOME/.rvm/scripts/rvm"
	fi

	__pathadd "$HOME/.rvm/bin"
fi
