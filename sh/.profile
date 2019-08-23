#!/usr/bin/env false

__rc="${__rc:-unknown}"
# ENV is read by dash to determine non-login shell init file
ENV="${HOME}/.profile"; export ENV

if test -t 1; then
	CLICOLOR='1'; export CLICOLOR
	EDITOR='emacs -nw'; export EDITOR
	LSCOLORS='ExFxCxDxBxegedabagacad'; export LSCOLORS
	VISUAL='emacs -nw'; export VISUAL

	__attr_red="$(tput setaf 1)"
	__attr_reset="$(tput sgr0)"

	__ps1_err() {
	    # shellcheck disable=SC2181
		if test "$?" -ne 0; then
			printf '%s' "${__attr_red}"
		fi
	}

	case "${__rc}" in
		zsh)
			case "${TERM}" in
				eterm*)
					PS1="%{${__attr_reset}%}%~ %{%(0?..${__attr_red})%}%(!.#.$)%{${__attr_reset}%} " ;;
				*)
					PS1="%{${__attr_reset}%(0?..${__attr_red})%}%(!.#.$)%{${__attr_reset}%} "
			esac ;;
		bash)
			case "${TERM}" in
				eterm*)
					PS1="\[${__attr_reset}\]\w \[\$(__ps1_err)\]\\$\[${__attr_reset}\] " ;;
				*)
					PS1="\[${__attr_reset}\$(__ps1_err)\]\\$\[${__attr_reset}\] "
			esac ;;
		*)
			PS1='$ '
	esac

	HISTCONTROL='ignoredups:erasedups'
	if test "$(command -v shopt)" = "shopt"; then
		shopt -s histappend
	fi

	if test -d "${HOME}/.config/sh-interactive"; then
		for __cmd in "${HOME}/.config/sh-interactive"/*; do
			. "${__cmd}"
		done
	fi
fi

__pathadd() {
	# Adds a path to $PATH only if it isn't already present
	# Pass `prepend` as a second argument to prepend to path
	case ":${PATH:=$1}:" in
		*:$1:*) ;;
		*)
			if test "$2" = 'prepend'; then
				PATH="$1:${PATH}"
			else
				PATH="${PATH}:$1"
			fi
	esac
}

if test -d "${HOME}/.cargo"; then
	CARGO_HOME="${HOME}/.cargo"; export CARGO_HOME
	__pathadd "${CARGO_HOME}/bin"
fi

if test -r "${HOME}/.nvm/nvm.sh"; then
	NVM_DIR="${HOME}/.nvm"; export NVM_DIR
	. "${NVM_DIR}/nvm.sh"
fi

if test -d "${HOME}/.rustup"; then
	RUSTUP_HOME="${HOME}/.rustup"; export RUSTUP_HOME
fi

if test -r "${HOME}/.rvm/scripts/rvm"; then
	. "${HOME}/.rvm/scripts/rvm"
	__pathadd "${HOME}/.rvm/bin"
fi
