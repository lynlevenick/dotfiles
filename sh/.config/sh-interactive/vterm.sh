#!/usr/bin/env false

if test -n "$TMUX" && (test "${TERM%%-*}" = "tmux" || test "${TERM%%-*}" = "screen"); then
	__vterm_printf() {
		# Tells tmux to pass the escape sequences through
		printf '\033Ptmux;\033\033]%s\007\033\\' "$1"
	}
elif test "${TERM%%-*}" = "screen"; then
	__vterm_printf() {
		# Tells GNU screen (screen, screen-256color, screen-256color-bce) to pass through
		printf '\033P\033]%s\007\033\\' "$1"
	}
else
	__vterm_printf() {
		printf '\033]%s\033\\' "$1"
	}
fi

__vterm_cmd() {
    __vterm_cmd_elisp=''
    while test $# -gt 0; do
        __vterm_cmd_elisp="$__vterm_cmd_elisp$(printf '"%s" ' "$(printf '%s' "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    __vterm_printf "51;E$__vterm_cmd_elisp"
}

__vterm_update_pwd() {
    __vterm_cmd lyn--vterm-update-pwd "$PWD/"
}

if test -n "$INSIDE_EMACS"; then
	PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}__vterm_update_pwd"
fi
