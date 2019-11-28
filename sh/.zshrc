#!/usr/bin/env false

if test -z "$__rc"; then
	__rc=zsh
fi

if test -r "$HOME/.shrc"; then
	. "$HOME/.shrc"
fi

## interactive zsh specific configuration follows

setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
HISTFILE="$HOME/.cache/zsh_history"
HISTSIZE=5000
SAVEHIST=5000

precmd() {
	unset CDPATH
	eval "$PROMPT_COMMAND"
}

case "$TERM" in
	eterm*)
		PS1="%{$__attr_reset%}%~ %{%(0?..$__attr_red)%}%(!.#.$)%{$__attr_reset%} " ;;
	*)
		PS1="%{$__attr_reset%(0?..$__attr_red)%}%(!.#.$)%{$__attr_reset%} "
esac

bindkey '^n' history-search-forward
bindkey '^p' history-search-backward
bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward
