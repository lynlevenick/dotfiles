function __lyn_visual_length --description\
    'Return length of string without color codes'

    echo $argv | string replace -ar '\x1b.*?[mGKH]' '' | string length
end
