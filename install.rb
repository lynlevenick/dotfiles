#! /usr/bin/ruby
abort 'Don\'t run this as root!' if Process.uid.zero?
STDIN.reopen('/dev/null')

class Dir
  def self.mkdirp(dirname)
    system('mkdir', '-p', dirname) || raise
  end
end

$SCRIPT_DIR = File.expand_path(File.dirname(__FILE__)).freeze

class BackupExists < IOError; end
$BACKUP_DIR = File.expand_path('backup', $SCRIPT_DIR).freeze
Dir.mkdirp($BACKUP_DIR)

def backup_and_symlink(old_name, new_name)
  if File.exist?(new_name)
    backup_name = File.expand_path(File.basename(new_name), $BACKUP_DIR)
    raise BackupExists if File.exist?(backup_name)

    File.rename(new_name, backup_name)
  end

  File.symlink(old_name, new_name)
end

$SECTION = []
def section(section)
  $SECTION.push(section)
  puts($SECTION.join(' '))

  yield
ensure
  $SECTION.pop
end

$OUTPUT_DIRECTORY = ''
$OUTPUT_PREFIX = ''
def with_directory(directory, prefix: '')
  old_directory = $OUTPUT_DIRECTORY
  old_prefix = $OUTPUT_PREFIX

  if !$OUTPUT_DIRECTORY.empty?
    $OUTPUT_DIRECTORY += '/' unless $OUTPUT_DIRECTORY[-1] == '/'
  end

  $OUTPUT_DIRECTORY += directory
  $OUTPUT_PREFIX = prefix

  Dir.mkdirp($OUTPUT_DIRECTORY)

  yield
ensure
  $OUTPUT_PREFIX = old_prefix
  $OUTPUT_DIRECTORY = old_directory
end

def dotfile(name)
  name_base = File.basename(name)
  backup_and_symlink(
   "#{$SCRIPT_DIR}/#{name}",
   "#{$OUTPUT_DIRECTORY}/#{$OUTPUT_PREFIX}#{name_base}"
  )
end

if __FILE__ == $0
  with_directory ENV['HOME'], prefix: '.' do
    section :homebrew do
      eval(`curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install`)
      system('brew', 'doctor') || raise
      system('brew', 'update') || raise

      section :bundle do
        dotfile 'Brewfile'
        system('brew', 'tap', 'homebrew/bundle') || raise
        system('brew', 'bundle', 'install', '--global') || raise
      end
    end

    section :bash do
      section :configuration do
        dotfile 'bash_profile'
        dotfile 'bashrc'
      end
    end

    section :fish do
      section :default_shell do
        require 'shellwords'

        fish_path = `which fish`
        system("echo #{fish_path.shellescape} | sudo tee -a /etc/shells >/dev/null") || raise
        system('sudo', 'chsh', '-u', ENV['USER'], '-s', fish_path) || raise
      end

      section :configuration do
        with_directory '.config' do
          dotfile 'fish'
        end
      end
    end

    section :git do
      section :configuration do
        dotfile 'gitconfig'
        dotfile 'gitignore'
      end
    end

    section :iterm2 do
      section :configuration do
        def configure(key, type, value)
          system('defaults', 'write', 'com.googlecode.iterm2.plist', key, "-#{type}", value.to_s) || raise
        end

        configure 'PrefsCustomFolder', 'string', "#{$SCRIPT_DIR}/iterm2"
        configure 'LoadPrefsFromCustomFolder', 'bool', true
      end
    end

    section :visual_studio_code do
      section :configuration do
        with_directory 'Library/Application Support/Code/User' do
          dotfile 'vscode/settings.json'
        end
      end

      section :extensions do
        def install_extension(ext)
          system('code', '--install-extension', ext) || raise
        end

        section :general do
          install_extension 'editorconfig.editorconfig'
          install_extension 'eamodio.gitlens'
          install_extension 'zhuangtongfa.material-theme'
          install_extension 'robertohuertasm.vscode-icons'
        end

        section :fish do
          install_extension 'skyapps.fish-vscode'
        end

        section :ruby do
          install_extension 'rebornix.ruby'
          install_extension 'karunamurti.haml'
        end
      end
    end
  end
end
