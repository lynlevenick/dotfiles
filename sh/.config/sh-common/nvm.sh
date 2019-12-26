#!/usr/bin/env false

if test -r "$HOME/.nvm/nvm.sh"; then
	NVM_DIR="$HOME/.nvm"; export NVM_DIR

	if test -t 1; then
		__node_commands='node npm nvm yarn'

		# Unnecessary printf makes this work in zsh and other shells too
		# shellcheck disable=SC2086
		for __command in $(printf '%s\n' $__node_commands); do
			# shellcheck disable=SC2139
			alias $__command="unalias $__node_commands && . \"\$NVM_DIR/nvm.sh\" && $__command"
		done
	else
		# shellcheck disable=SC1090
		. "$NVM_DIR/nvm.sh"
	fi
fi
