#!/usr/bin/env false

# bash will source only bashrc for interactive, non-login
# shells - if __rc is unset, we haven't run profile yet
# and need to source it

if test -z "${__rc}"; then
	__rc='bash'

	if test -r "${HOME}/.profile"; then
		. "${HOME}/.profile"
	fi
fi

## bash-specific configuration follows

HISTCONTROL='ignoredups:erasedups'
HISTFILE="${HOME}/.cache/bash_history"
HISTFILESIZE='5000'
HISTSIZE='5000'
shopt -s histappend

case "${TERM}" in
	eterm*)
		PS1="\[${__attr_reset}\]\w \[\$(__ps1_err)\]\\$\[${__attr_reset}\] " ;;
	*)
		PS1="\[${__attr_reset}\$(__ps1_err)\]\\$\[${__attr_reset}\] "
esac
