function __lyn_append --no-scope-shadowing --description\
    'Append the second argument to the first, as like set'

    set $argv[1] "$$argv[1]""$argv[2]"
end
