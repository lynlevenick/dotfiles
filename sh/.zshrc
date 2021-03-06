#!/usr/bin/env false

if test -z "$__rc"; then
	__rc=zsh
fi

if test -r "$HOME/.shrc"; then
	# shellcheck source=./.shrc
	. "$HOME/.shrc"
fi

## interactive zsh specific configuration follows

setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
mkdir -p "$__cache_home/zsh"
HISTFILE="$__cache_home/zsh/history"
HISTSIZE=5000
SAVEHIST=5000

precmd() {
	unset CDPATH
	eval "$PROMPT_COMMAND"
}

case "$INSIDE_EMACS" in
	'')
		PS1="%{$__attr_unset%(0?.$__attr_dim$__attr_white.$__attr_red)%}%(!.#.;) %{$__attr_reset%}" ;;
	*vterm*)
		PS1="%{$__attr_unset%}%~ %{%(0?.$__attr_dim$__attr_white.$__attr_red)%}%(!.#.;) %{$(__vterm_printf '51;A')$__attr_reset%}" ;;
	*)
		PS1="%{$__attr_unset%}%~ %{%(0?.$__attr_dim$__attr_white.$__attr_red)%}%(!.#.;) %{$__attr_reset%}"
esac

bindkey '^n' history-search-forward
bindkey '^p' history-search-backward
bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward

autoload -U select-word-style
select-word-style bash
