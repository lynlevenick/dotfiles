#!/usr/bin/env sh

cli_cols="$(tput cols)"
cli_reset_line="\015$(tput el)"

attr_dim="$(tput dim)"
attr_green="$(tput setaf 2)"
attr_red="$(tput setaf 1)"
attr_reset="$(tput sgr0)$(tput cnorm)"
attr_white="$(tput setaf 7)"

out_pass="${attr_green}\342\234\223${attr_reset}"
out_fail="${attr_red}\342\234\227${attr_reset}"

## Output fundamentals

unset last_out_overwrite

out_step() {
	if test -n "$last_out_overwrite" -ne 0; then
		printf "${cli_reset_line}"
	fi
	last_out_overwrite=1
}

out_complete() {
	if test -n "$last_out_overwrite" -ne 0; then
		printf "${cli_reset_line}"
	fi
	unset last_out_overwrite
}

## Status updates

status_pass() {
	out_complete
	printf " [${out_pass}] %s ${attr_dim}${attr_white}%*s${attr_reset}\n" "$1" "$((cli_cols - ${#1} - 7))" "$2"
}

status_fail() {
	out_complete
	printf " [${out_fail}] %s ${attr_dim}${attr_white}%*s${attr_reset}\n" "$1" "$((cli_cols - ${#1} - 7))" "$2" 1>&2
	exit 1
}

status_update() {
	out_step
	printf " [ ] %s ${attr_dim}${attr_white}%*s${attr_reset}" "$1" "$((cli_cols - ${#1} - 7))" "$2"
}

status_other() {
	out_step
	printf " ... %s ${attr_dim}${attr_white}%*s${attr_reset}" "$1" "$((cli_cols - ${#1} - 7))" "$2"
}

## Helpers

script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)"

task_async() {
	# Parse arguments - everything before -- is the command to run,
	# everything after describes the job
	task_command=''

	while :; do
		case "$1" in
			--)
				shift
				break ;;
			*)
				task_command="${task_command:+$task_command }$1"
		esac

		shift
	done

	if test ! -e "${2:-/usr/local/bin/$1}"; then
		status_update "$1" 'installing...'
		status_dir="$(mktemp -d)"

		{
			if ! $task_command -- "$1" </dev/null >"${status_dir}/stdout" 2>"${status_dir}/stderr"; then
				printf "$?" > "${status_dir}/errno"
			fi
		} &

		status_pid="$!"
		wait "${status_pid}"

		if test -e "${status_dir}/errno"; then
			status_fail "$1" "failed - see ${status_dir}"
		else
			status_pass "$1" 'installed'
		fi
	else
		status_pass "$1" 'installed'
	fi
}

task_brew() {
	task_async brew install -- "$@"
}

task_cask() {
	task_async brew cask install -- "$@"
}

task_stow() {
	status_other "$1" 'configuring...'
	printf ' '

	find "${script_dir}/$1" -type f -exec /usr/bin/env sh -c '
		target="${HOME}/${1#$2/}"
		if test ! -h "${target}"; then
			if test -e "${target}"; then
				printf '\''     Warning: %s exists\n'\'' "${target}"
			else
				mkdir -p "$(dirname -- "${target}")"
				ln -s "$1" "$target"
			fi
		fi
	' _ {} "${script_dir}/$1" \;

	status_pass "$1" 'configured'
}

## Installation

if test "0$(id -u)" -eq 0; then
	status_fail 'Do not install these dotfiles as root'
fi

# Brew should be the one interactive element of this install process,
# so let it do whatever with the terminal
if ! command -v brew >/dev/null; then
	status_other 'Homebrew' 'installing...'
	# TODO: Update method when macOS removes builtin ruby
	curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install | ruby

	if command -v brew >/dev/null; then
		status_pass 'Homebrew' 'installed'
	else
		status_fail 'Homebrew' 'failed'
	fi
else
	status_pass 'Homebrew' 'installed'
fi

## Command line tooling

task_brew bat

task_brew diff-so-fancy

task_brew fzf

task_stow git

task_brew python '/usr/local/bin/python3'

task_stow readline

task_brew ripgrep '/usr/local/bin/rg'

task_stow sh

task_stow ssh

## Applications

task_stow alacritty
task_cask alacritty '/Applications/Alacritty.app'

task_stow emacs
task_cask emacs '/Applications/Emacs.app'

task_cask firefox-nightly '/Applications/Firefox Nightly.app'

## Fonts

task_cask homebrew/cask-fonts/font-symbola "${HOME}/Library/Fonts/Symbola_Hinted.ttf"

task_cask homebrew/cask-fonts/font-go-mono-nerd-font "${HOME}/Library/Fonts/Go Mono Nerd Font Complete.ttf"
