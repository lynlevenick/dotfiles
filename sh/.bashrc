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

if test -r "${HOME}/.shrc"; then
	. "${HOME}/.shrc"
fi

## bash-specific configuration follows

__attr_red="$(tput setaf 1)"
__attr_reset="$(tput sgr0)"

HISTCONTROL='ignoredups:erasedups'
HISTFILE="${HOME}/.cache/bash_history"
HISTSIZE='5000'
shopt -s histappend

__ps1_err() {
	# shellcheck disable=SC2181
	if test "$?" -ne 0; then
		printf '%s' "${__attr_red}"
	fi
}

case "${TERM}" in
	eterm*)
		PS1="\[${__attr_reset}\]\w \[\$(__ps1_err)\]\\$\[${__attr_reset}\] " ;;
	*)
		PS1="\[${__attr_reset}\$(__ps1_err)\]\\$\[${__attr_reset}\] "
esac
