function fish_prompt --description\
    'Write the prompt'
    set -l last_status $status

    set -l right_prompt (__fish_vcs_prompt)
    set -l right_length (__lyn_visual_length $right_prompt)

    set -l left_prompt ''

    if test $last_status -ne 0
        __lyn_append left_prompt (set_color $fish_color_error)
    end
    __lyn_append left_prompt $USER
    __lyn_append left_prompt '@'
    __lyn_append left_prompt (prompt_hostname)
    __lyn_append left_prompt (set_color normal)
    __lyn_append left_prompt ' '

    switch $USER
        case root toor
            __lyn_append left_prompt (set_color $fish_color_cwd_root)
        case '*'
            __lyn_append left_prompt (set_color $fish_color_cwd)
    end
    set -l should_erase_fish_prompt_pwd_dir_length
    if not set -q fish_prompt_pwd_dir_length
        set -l left_length (__lyn_visual_length $left_prompt)
        set -g fish_prompt_pwd_dir_length 0
        set -l prompt_pwd_length (__lyn_visual_length (prompt_pwd))

        if test (math $COLUMNS - $left_length - $right_length - $prompt_pwd_length) -le 0
            set -e fish_prompt_pwd_dir_length
            set -e should_erase_fish_prompt_pwd_dir_length
        end
    else
        set -e should_erase_fish_prompt_pwd_dir_length
    end
    __lyn_append left_prompt (prompt_pwd)
    if set -q should_erase_fish_prompt_pwd_dir_length
        set -e fish_prompt_pwd_dir_length
    end
    __lyn_append left_prompt (set_color normal)

    set -l left_length (__lyn_visual_length $left_prompt)

    set -l spaces (math $COLUMNS - $left_length - $right_length)
    if test $spaces -gt 0
        echo -n $left_prompt
        string repeat -Nn $spaces ' '
        echo $right_prompt
    else
        echo $left_prompt
    end

    switch $USER
        case root toor
            echo -n '# '
        case '*'
            echo -n '> '
    end
end
