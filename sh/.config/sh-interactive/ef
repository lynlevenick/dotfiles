#!/usr/bin/env false

__ef_action() {
    if test -n "${1%??}"; then
        ${VISUAL:-${EDITOR:-vi}} -- "${1%??}"
    fi
}

if command -v highlight >/dev/null; then
    __ef_highlighter='(highlight --out-format=ansi {} 2>/dev/null || cat {})'
else
    __ef_highlighter='cat {}'
fi

ef() {
    __ef_action "$(
        (rg --files --hidden --null 2>/dev/null |
         fzf-tmux --read0 --select-1 --query="$*" \
                  --bind='?:toggle-preview' \
                  --preview='
                      case "$(file --mime {})" in
                          *binary*)
                              printf '\''%s: binary file'\'' {} ;;
                          *)
                              '"${__ef_highlighter}"' |
                              head -n "$((LINES * 4))"
                      esac' \
                  --preview-window=hidden
        ) && printf '$'
    )"
}
