#!/usr/bin/env false

__dynamic_title() {
    __dynamic_title_pwd="$(printf '%s' "${PWD}" | sed 's|^'"${HOME}"'|~|')"
    printf '\033]0;%s\007' "${__dynamic_title_pwd}"
}

case "${TERM}" in
    xterm*|alacritty)
        PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;}"'__dynamic_title' ;;
    *)
esac
