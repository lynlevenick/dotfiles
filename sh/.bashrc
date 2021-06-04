#!/usr/bin/env false

if test -z "$__rc"; then
	__rc=bash
fi

if test -r "$HOME/.shrc"; then
	# shellcheck source=./.shrc
	. "$HOME/.shrc"
fi

## interactive bash specific configuration follows

HISTCONTROL=ignoredups:erasedups
HISTFILE="$HOME/.cache/bash_history"
HISTFILESIZE=5000
HISTSIZE=5000
shopt -s histappend

case "$INSIDE_EMACS" in
	'')
		PS1="\[$__attr_unset\$(__ps1_err)\]\$(__ps1_prompt_chr) \[$__attr_reset\]" ;;
	*vterm*)
		PS1="\[$__attr_unset\]\w \[\$(__ps1_err)\]\$(__ps1_prompt_chr) \[\$(__vterm_printf '51;A')$__attr_reset\]" ;;
	*)
		PS1="\[$__attr_unset\]\w \[\$(__ps1_err)\]\$(__ps1_prompt_chr) \[$__attr_reset\]"
esac
