abort 'Don\'t run this as root!' if Process.uid.zero?

require 'pathname'

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
  end

  targets
end

task default: [:bash, :git, :homebrew, :readline, :ssh, :vscode]

task bash: [*stow($pwd.join('bash'))]

task git: [*stow($pwd.join('git'))]

task homebrew: ['/usr/local/bin/brew', *stow($pwd.join('homebrew'))] do
  sh 'brew', 'doctor'
  sh 'brew', 'update'
  sh 'brew', 'tap', 'homebrew/bundle'
  sh 'brew', 'bundle', 'install', '--global'
end
file '/usr/local/bin/brew' do
  eval(%x{curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install})
end

task readline: [*stow($pwd.join('readline'))]

task ssh: [*stow($pwd.join('ssh'))]

task vscode: [:homebrew, *stow($pwd.join('vscode'))] do
  sh 'code', '--install-extension', 'editorconfig.editorconfig'
  sh 'code', '--install-extension', 'eamodio.gitlens'
  sh 'code', '--install-extension', 'zhuangtongfa.material-theme'
  sh 'code', '--install-extension', 'robertohuertasm.vscode-icons'
end
