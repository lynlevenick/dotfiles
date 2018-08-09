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
        ln -si "${script_dir}/bashrc" "${HOME}/.bashrc" || true
        ln -si "${script_dir}/Brewfile" "${HOME}/.Brewfile" || true
        ln -si "${script_dir}/gitconfig" "${HOME}/.gitconfig" || true
        ln -si "${script_dir}/gitignore" "${HOME}/.gitignore" || true

        echo -n 'Install packages (Y/n) => '; read answer
        if [ "$answer" != 'n' ] && [ "$answer" != 'N' ] ; then
            sudo chown -R "${USER}:admin" /usr/local
            /usr/bin/env ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

            brew doctor
            brew update

            brew tap homebrew/bundle
            brew bundle install --global

            echo -n 'Switch user shell to fish (Y/n) => '; read answer
            if [ "$answer" != 'n' ] && [ "$answer" != 'N' ] ; then
                echo '/usr/local/bin/fish' | sudo tee -a /etc/shells >/dev/null
                sudo chsh -u "$USER" -s '/usr/local/bin/fish'
            fi

            echo -n 'Install vscode settings (Y/n) => '; read answer
            if [ "$answer" != 'n' ] && [ "$answer" != 'N' ] ; then
                mkdir -p "${HOME}/Library/Application Support/Code/User"
                ln -si "${script_dir}/vscode/settings.json" "${HOME}/Library/Application Support/Code/User/settings.json"

                echo -n 'Install vscode extensions (Y/n) => '; read answer
                if [ "$answer" != 'n' ] && [ "$answer" != 'N' ] ; then
                    code --install-extension 'editorconfig.editorconfig'
                    code --install-extension 'eamodio.gitlens'
                    code --install-extension 'zhuangtongfa.material-theme'
                    code --install-extension 'robertohuertasm.vscode-icons'

                    # Ruby
                    code --install-extension 'rebornix.ruby'
                    code --install-extension 'karunamurti.haml'
                fi
            fi
        fi
    fi
}

main $@
