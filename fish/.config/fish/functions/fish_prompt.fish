function fish_prompt --description 'Very plain prompt.'
  set -l color_prompt

  if test $status -ne 0
    set color_prompt $fish_color_error
  else
    set color_prompt normal
  end

  set -l prompt_character
  switch $USER
    case root toor
      set prompt_character '#'
    case '*'
      set prompt_character '$'
  end

  printf "%s%s %s" (set_color $color_prompt) "$prompt_character" (set_color normal)
end
