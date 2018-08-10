function fish_title --description\
    'Write the title'

    set -l home_dir ~
    echo $_ (string replace -r '^'"$home_dir"'($|/)' '~$1' $PWD)
end
