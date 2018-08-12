export CLICOLOR=1
export EDITOR='code --wait'

reset_colors=$(tput sgr0)
export PS1="${reset_colors}\\$ "
