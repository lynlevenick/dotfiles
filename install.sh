#! /usr/bin/env sh

set -o errexit
set -o nounset
set -o pipefail

main() {
    local answer
    local script_dir=$(pwd)/$(dirname "$0")

    echo -n 'Link dotfiles (Y/n) => '; read answer
    if [ "$answer" != 'n' ] && [ "$answer" != 'N' ] ; then
        mkdir -p "${HOME}/.config"

        ln -si "${script_dir}/bash_profile" "${HOME}/.bash_profile" || true
        ln -si "${script_dir}/Brewfile" "${HOME}/.Brewfile" || true
        ln -si "${script_dir}/gitconfig" "${HOME}/.gitconfig" || true
        ln -si "${script_dir}/gitignore" "${HOME}/.gitignore" || true
    fi

    echo -n 'Install all base packages (Y/n) => '; read answer
    if [ "$answer" != 'n' ] && [ "$answer" != 'N' ] ; then
        sudo chown -R "${USER}:admin" /usr/local
        /usr/bin/env ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

        brew doctor
        brew update

        brew tap homebrew/bundle
        brew bundle install --global
    fi

    echo -n 'Switch user shell to fish (Y/n) => '; read answer
    if [ "$answer" != 'n' ] && [ "$answer" != 'N' ] ; then
        echo '/usr/local/bin/fish' | sudo tee -a /etc/shells >/dev/null
        sudo chsh -u "$USER" -s '/usr/local/bin/fish'
    fi
}

main "$@"