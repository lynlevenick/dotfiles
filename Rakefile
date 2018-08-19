abort 'Don\'t run this as root!' if Process.uid.zero?

require 'pathname'
require 'rake/clean'

$home = Pathname.new(ENV['HOME'])
$pwd = Pathname.new(__FILE__).dirname

def stow(dir)
  dir = Pathname.new(dir) unless dir.is_a?(Pathname)

  targets = []
  Dir.glob(dir.join('**/*'), File::FNM_DOTMATCH) do |source_path|
    source_path = Pathname.new(source_path)
    next unless source_path.file?

    relative_dirname = source_path.dirname.relative_path_from(dir)
    target_path = $home.join(relative_dirname).join(source_path.basename)

    file target_path => source_path do |task|
      target = Pathname.new(task.name)
      source = Pathname.new(task.source)

      if target.exist?
        next if target.symlink? && target.realpath.cleanpath == source.cleanpath
        raise "fatal: File exists: #{target}"
      end

      target.dirname.mkpath
      target.make_symlink(source)
    end

    targets << target_path.to_s
    CLOBBER << target_path.to_s
  end

  targets
end

task default: [:bash, :git, :homebrew, :readline, :ssh, :vscode]

bash_files = stow($pwd.join('bash'))
desc 'Configure bash'
task bash: [*bash_files]

git_files = stow($pwd.join('git'))
desc 'Configure git'
task git: [*git_files]

homebrew_files = stow($pwd.join('homebrew'))
desc 'Configure homebrew'
task homebrew: ['/usr/local/bin/brew', *homebrew_files] do
  sh 'brew', 'doctor'
  sh 'brew', 'update'
  sh 'brew', 'tap', 'homebrew/bundle'
  sh 'brew', 'bundle', 'install', '--global'
end
file '/usr/local/bin/brew' do
  eval(%x{curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install})
end

readline_files = stow($pwd.join('readline'))
desc 'Configure readline'
task readline: [*readline_files]

ssh_files = stow($pwd.join('ssh'))
desc 'Configure ssh'
task ssh: [*ssh_files]

vscode_files = stow($pwd.join('vscode'))
desc 'Configure vscode'
task vscode: [:homebrew, *vscode_files] do
  sh 'code', '--install-extension', 'editorconfig.editorconfig'
  sh 'code', '--install-extension', 'eamodio.gitlens'
  sh 'code', '--install-extension', 'zhuangtongfa.material-theme'
  sh 'code', '--install-extension', 'robertohuertasm.vscode-icons'
end
