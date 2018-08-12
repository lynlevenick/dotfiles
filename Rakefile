abort 'Don\'t run this as root!' if Process.uid.zero?
STDIN.reopen('/dev/null')

$home = File.expand_path(ENV['HOME']).freeze
$pwd = File.expand_path(File.dirname(__FILE__)).freeze

class BackupExists < IOError; end
$backup = File.expand_path('backup', pwd).freeze
def link_and_backup(old_name, new_name)
  sh 'mkdir', '-p', File.dirname(new_name)

  if File.exist?(new_name)
    backup_name = File.expand_path(File.basename(new_name), $backup)
    raise BackupExists if File.exist?(backup_name)

    sh 'mkdir', '-p', $backup
    sh 'mv', new_name, backup_name
  end

  sh 'ln', '-s', old_name, new_name
end

task :default => [:apps, :configuration, :dotfiles]

task :apps => [:homebrew, :dotfiles] do
  sh 'brew', 'tap', 'homebrew/bundle'
  sh 'brew', 'bundle', 'install', '--global'
end
task :configuration => [:vscode_config]
task :dotfiles => [
  :bash_dotfiles,
  :git_dotfiles,
  :homebrew_dotfiles,
  :hyper_dotfiles,
  :ssh_dotfiles,
  :vscode_dotfiles,
]

task :vscode_config => [:apps] do
  sh 'code', '--install-extension', 'editorconfig.editorconfig'
  sh 'code', '--install-extension', 'eamodio.gitlens'
  sh 'code', '--install-extension', 'zhuangtongfa.material-theme'
  sh 'code', '--install-extension', 'robertohuertasm.vscode-icons'
  sh 'code', '--install-extension', 'rebornix.ruby'
  sh 'code', '--install-extension', 'karunamurti.haml'
end

task :bash_dotfiles => ["#{$home}/.bash_profile", "#{$home}/.bashrc"]
task :git_dotfiles => ["#{$home}/.gitconfig", "#{$home}/.gitignore"]
task :ssh_dotfiles => ["#{$home}/.ssh"]
task :homebrew_dotfiles => ["#{$home}/.Brewfile"]
task :hyper_dotfiles => ["#{$home}/.hyper.js"]

vscode_settings = File.expand_path('Library/Application Support/Code/User/settings.json', $home)
task :vscode_dotfiles => [vscode_settings]
file vscode_settings => [File.expand_path('vscode/settings.json', pwd)] do |t|
  link_and_backup(t.source, t.name)
end

task :homebrew => ['/usr/local/bin/brew'] do
  sh 'brew', 'doctor'
  sh 'brew', 'update' do
  end
end
file '/usr/local/bin/brew' do
  success, script = sh 'curl', '-fsSL', 'https://raw.githubusercontent.com/Homebrew/install/master/install'
  success || raise
  eval script
end

rule %r{\A#{Regexp.escape($home)}\/\..+\z} => proc { |task_name|
  dotfile_name = task_name[($home.size + 1)..-1]
  File.expand_path(File.basename(dotfile_name[1..-1]), $pwd)
} do |t|
  target = File.expand_path(t.name, $home)
  link_and_backup(t.source, target)
end
