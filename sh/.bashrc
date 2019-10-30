#!/usr/bin/env false

if test -z "${__rc}"; then
	__rc='bash'
fi

if test -r "${HOME}/.shrc"; then
	. "${HOME}/.shrc"
fi

## interactive bash specific configuration follows

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
