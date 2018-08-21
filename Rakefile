# frozen_string_literal: true

abort "Don't run this as root!" if Process.uid.zero?

require "open3"
require "pathname"
require "rake/clean"

# Create rake tasks that emulate GNU Stow
module Stow
  extend Rake::DSL

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
    def sources(from)
      from.glob("**/*", File::FNM_DOTMATCH)
          .map(&Pathname.method(:new))
          .select(&:file?)
    end

    def target(source, from, into)
      dirname = source.dirname.relative_path_from(from)
      into.join(dirname).join(source.basename)
    end

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
task default: %i[configure install stow]

desc "Perform non-file configuration"
task configure: :'configure:default'
namespace :configure do
  task default: %i[vscode]

  task vscode: %i[install:homebrew] do
    sh "code", "--install-extension", "editorconfig.editorconfig"
    sh "code", "--install-extension", "eamodio.gitlens"
    sh "code", "--install-extension", "zhuangtongfa.material-theme"
    sh "code", "--install-extension", "robertohuertasm.vscode-icons"
  end
end

desc "Install programs"
task install: :'install:default'
namespace :install do
  task default: %i[gems homebrew]

  task gems: %i[homebrew] do
    Dir.chdir(PWD) do
      sh "bundle", "install", "--path", ".bundle"
    end
  end

  task homebrew: [:'stow:homebrew', "/usr/local/bin/brew"] do
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

desc "Link configuration files emulating GNU Stow"
task stow: :'stow:default'
namespace :stow do
  task default: %i[bash git git_hooks homebrew readline ssh vscode]

  task bash:      [*Stow.stow(PWD.join("bash"))]
  task git:       [*Stow.stow(PWD.join("git"))]
  task git_hooks: [*Stow.stow(PWD.join("git_hooks"),
                              into: PWD.join(".git/hooks"))]
  task homebrew:  [*Stow.stow(PWD.join("homebrew"))]
  task readline:  [*Stow.stow(PWD.join("readline"))]
  task ssh:       [*Stow.stow(PWD.join("ssh"))]
  task vscode:    [*Stow.stow(PWD.join("vscode"))]
end

namespace :run do
  desc "Run rubocop"
  task rubocop: %i[install:gems stow:git_hooks] do
    Dir.chdir(PWD) do
      sh "bundle", "exec", "rubocop"
    end
  end
end
