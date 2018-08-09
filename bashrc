EDITOR=code --wait

term_default_bg__() {
    echo -en "\033]6;1;bg;red;brightness;40\a"
    echo -en "\033]6;1;bg;green;brightness;44\a"
    echo -en "\033]6;1;bg;blue;brightness;52\a"
}

term_default_bg__
