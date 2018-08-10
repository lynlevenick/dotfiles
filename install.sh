#! /usr/bin/env sh

set -o errexit
set -o nounset
set -o pipefail

backup_link() {
    if [ -e "$2" ] ; then
        echo "Backing up '$2'"
        mkdir -p "${script_dir}/backup/"

        local target_basename=$(basename "$2")
        local target="${script_dir}/backup/${target_basename}"

        if [ -e "${target}" ] ; then
            echo "Backup of '$2' already exists! Exiting"
            exit 1
        fi
        mv "$2" "${target}"
    fi

    echo "Linking '$2' to '$1'"
    ln -s "$1" "$2"
}

main() {
    local answer
    local script_dir=$(pwd)/$(dirname "$0")

    backup_link "${script_dir}/bash_profile" "${HOME}/.bash_profile"
    backup_link "${script_dir}/bashrc" "${HOME}/.bashrc"
    backup_link "${script_dir}/Brewfile" "${HOME}/.Brewfile"
    backup_link "${script_dir}/gitconfig" "${HOME}/.gitconfig"
    backup_link "${script_dir}/gitignore" "${HOME}/.gitignore"
    mkdir -p "${HOME}/.config"
    backup_link "${script_dir}/fish" "${HOME}/.config/fish"

    echo 'Installing homebrew'
    sudo chown -R "${USER}:admin" /usr/local
    /usr/bin/env ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" <&-

    brew doctor
    brew update

    echo 'Installing packages'
    brew tap homebrew/bundle
    brew bundle install --global

    echo 'Setting fish as default shell'
    echo "$(which fish)" | sudo tee -a /etc/shells >/dev/null
    sudo chsh -u "${USER}" -s "$(which fish)"

    echo 'Setting iterm2 configuration directory'
    defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "${script_dir}/iterm2"
    defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true

    echo 'Installing vscode settings'
    mkdir -p "${HOME}/Library/Application Support/Code/User"
    backup_link "${script_dir}/vscode/settings.json" "${HOME}/Library/Application Support/Code/User/settings.json"

    echo 'Installing vscode general extensions'
    code --install-extension 'editorconfig.editorconfig'
    code --install-extension 'eamodio.gitlens'
    code --install-extension 'zhuangtongfa.material-theme'
    code --install-extension 'robertohuertasm.vscode-icons'

    echo 'Installing vscode fish extensions'
    code --install-extension 'skyapps.fish-vscode'

    echo 'Installing vscode ruby extensions'
    code --install-extension 'rebornix.ruby'
    code --install-extension 'karunamurti.haml'
}

main "$@"
