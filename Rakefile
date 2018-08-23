# frozen_string_literal: true

abort "Don't run this as root!" if Process.uid.zero?

require "open3"
require "pathname"
require "rake/clean"

# Create rake tasks that emulate GNU Stow
module Stow
  extend Rake::DSL

  # Creates a set of rake tasks that will link files from the source to
  # destination.
  #
  # @param [Pathname, String] from the directory files will be linked from
  # @param [Pathname, String] into the directory files will be linked into
  # @return [Array<String>] names of targets, usable by rake
  def self.stow(from, into: ENV["HOME"])
    from = Pathname.new(from).expand_path
    into = Pathname.new(into).expand_path

    sources = Stow.sources(from)
    targets = sources.map { |source| Stow.target(source, from, into) }

    sources.zip(targets).each do |source, target|
      Stow.symlink(target, source)
    end

    CLOBBER.concat(targets)
    targets
  end

  class << self
    # Retrieves a list of files in a directory.
    #
    # @param [Pathname] from directory to list
    # @return [Array<Pathname>] files in directory
    def sources(from)
      from.glob("**/*", File::FNM_DOTMATCH)
          .map(&Pathname.method(:new))
          .select(&:file?)
    end

    # Transforms a source filename in from into a destination filename
    # in into
    #
    # @param [Pathname] source the file to transform
    # @param [Pathname] from the directory to transform from
    # @param [Pathname] into the directory to transform into
    def target(source, from, into)
      dirname = source.dirname.relative_path_from(from)
      into.join(dirname).join(source.basename)
    end

    # Creates a rake task to symlink target to source
    #
    # @param [Pathname, String] target destination file location
    # @param [Pathname, String] source source file location
    def symlink(target, source)
      file target => source do |task|
        target = Pathname.new(task.name)
        source = Pathname.new(task.source)

        if target.exist?
          next if target.symlink? && target.realpath == source.cleanpath
          raise "fatal: File exists: #{target}"
        end

        target.dirname.mkpath
        target.make_symlink(source)
      end
    end
  end
end

PWD = Pathname.new(__FILE__).dirname.freeze

desc "Perform all tasks"
task :default => %i[configure install stow]

desc "Perform non-file configuration"
task configure: %i[configure:visual-studio-code]
namespace :configure do
  task :'visual-studio-code' => %i[install:homebrew] do
    sh "code", "--install-extension", "editorconfig.editorconfig"
    sh "code", "--install-extension", "eamodio.gitlens"
    sh "code", "--install-extension", "zhuangtongfa.material-theme"
    sh "code", "--install-extension", "rebornix.ruby"
    sh "code", "--install-extension", "castwide.solargraph"
    sh "code", "--install-extension", "robertohuertasm.vscode-icons"
  end
end

desc "Install programs"
task install: %i[install:dev_gems install:gems install:homebrew]
namespace :install do
  task :dev_gems => [:homebrew] do
    Dir.chdir(PWD) do
      sh "bundle", "install", "--path", ".bundle"
    end
  end

  task :gems => [:homebrew] do
    sh "gem", "install", "solargraph"
  end

  task :homebrew => ["/usr/local/bin/brew"] do
    sh "brew", "doctor"
    sh "brew", "update"
    sh "brew", "tap", "homebrew/bundle"
    sh "brew", "bundle", "install", "--global"
  end
  file "/usr/local/bin/brew" do
    installed = Open3.pipeline(
      ["curl", "-fsSL", "https://raw.githubusercontent.com/Homebrew/install/master/install"],
      "ruby",
    ).all(&:zero?)

    raise "fatal: Homebrew install failed" unless installed
  end
end

desc "Link configuration files"
task stow: %i[stow:bash stow:git stow:homebrew stow:readline stow:ssh
              stow:visual-studio-code]
namespace :stow do
  task :bash =>                 [*Stow.stow(PWD.join("bash"))]
  task :git =>                  [*Stow.stow(PWD.join("git"))]
  task :homebrew =>             [*Stow.stow(PWD.join("homebrew"))]
  task :readline =>             [*Stow.stow(PWD.join("readline"))]
  task :ssh =>                  [*Stow.stow(PWD.join("ssh"))]
  task :'visual-studio-code' => [*Stow.stow(PWD.join("visual-studio-code"))]
end
