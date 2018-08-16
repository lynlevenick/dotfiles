abort 'Don\'t run this as root!' if Process.uid.zero?

require 'pathname'

$home = Pathname.new(ENV['HOME'])
$pwd = Pathname.new(__FILE__).dirname

def stow(dir)
  dir = Pathname.new(dir) unless dir.is_a?(Pathname)

  targets = []
  Dir.glob(dir.join('**/*'), File::FNM_DOTMATCH) do |path|
    path = Pathname.new(path)
    next unless path.file?

    relative_dirname = path.dirname.relative_path_from(dir)
    target = $home.join(relative_dirname).join(path.basename)

    file target => path do
      if target.exist?
        next if target.symlink? && target.realpath.cleanpath == path.cleanpath
        raise "fatal: File exists: #{target}"
      end

      target.dirname.mkpath
      target.make_symlink(path)
    end

    targets << target
  end

  targets
end

multitask :default => [:bash, :git, :homebrew, :readline, :ssh, :vscode]

multitask :bash => [*stow($pwd.join('bash'))]

multitask :git => [*stow($pwd.join('git'))]

multitask :homebrew => ['/usr/local/bin/brew', *stow($pwd.join('homebrew'))] do
  sh 'brew', 'doctor'
  sh 'brew', 'update'
  sh 'brew', 'tap', 'homebrew/bundle'
  sh 'brew', 'bundle', 'install', '--global'
end
file '/usr/local/bin/brew' do
  eval(%x{curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install})
end

multitask :readline => [*stow($pwd.join('readline'))]

multitask :ssh => [*stow($pwd.join('ssh'))]

multitask :vscode => [:homebrew, *stow($pwd.join('vscode'))] do
  sh 'code', '--install-extension', 'editorconfig.editorconfig'
  sh 'code', '--install-extension', 'eamodio.gitlens'
  sh 'code', '--install-extension', 'zhuangtongfa.material-theme'
  sh 'code', '--install-extension', 'robertohuertasm.vscode-icons'
  sh 'code', '--install-extension', 'rebornix.ruby'
  sh 'code', '--install-extension', 'karunamurti.haml'
end
