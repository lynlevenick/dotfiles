# frozen_string_literal: true

abort "Don't run this as root!" if Process.uid.zero?

require "open3"
require "pathname"

# Create rake tasks that emulate GNU Stow
module Stow
  extend Rake::DSL

  # Create a set of rake tasks that will link all files from the source
  # directory to the destination directory
  def self.stow(from, into: ENV["HOME"])
    from = Pathname.new(from).expand_path
    into = Pathname.new(into).expand_path

    sources = Stow.sources(from)
    targets = sources.map { |source| Stow.target(source, from, into) }

    sources.zip(targets).each do |source, target|
      Stow.symlink(target, source)
    end

    targets
  end

  class << self
    # Retrieve a list of files in a directory
    def sources(from)
      Dir.glob(from.join("**/*"), File::FNM_DOTMATCH)
         .map(&Pathname.method(:new))
         .select(&:file?)
    end

    # Transform a source filename into a destination filename
    def target(source, from, into)
      dirname = source.dirname.relative_path_from(from)
      into.join(dirname).join(source.basename)
    end

    # Create a rake task to symlink target to source
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

desc "Install and configure all programs"
task :default => %i[bash emacs git homebrew python readline ripgrep ruby ssh]

bash_files = Stow.stow(PWD.join("bash"))
desc "Configure bash"
task :bash => [*bash_files]

emacs_files = Stow.stow(PWD.join("emacs"))
desc "Install and configure emacs"
task :emacs => ["/Applications/Emacs.app",
                *emacs_files]
file "/Applications/Emacs.app" => ["/usr/local/bin/brew"] do
  sh "brew", "cask", "install", "emacs"
  sh "touch", "-c", "/Applications/Emacs.app"
end

git_files = Stow.stow(PWD.join("git"))
desc "Configure git"
task :git => [*git_files]

desc "Install homebrew"
task :homebrew => ["/usr/local/bin/brew"]
file "/usr/local/bin/brew" do
  Open3.pipeline(
    ["curl", "-fsSL", "https://raw.githubusercontent.com/Homebrew/install/master/install"],
    "ruby",
  ).all(&:zero?) or raise "fatal: Homebrew install failed"

  sh "brew", "doctor"
  sh "brew", "update"
  sh "touch", "-c", "/usr/local/bin/brew"
end

desc "Install python"
task :python => ["/usr/local/bin/python3"]
file "/usr/local/bin/python3" => "/usr/local/bin/brew" do
  sh "brew", "install", "python"
  sh "touch", "-c", "/usr/local/bin/python3"
end

readline_files = Stow.stow(PWD.join("readline"))
desc "Configure readline"
task :readline => [*readline_files]

desc "Install ripgrep"
task :ripgrep => ["/usr/local/bin/rg"]
file "/usr/local/bin/rg" => "/usr/local/bin/brew" do
  sh "brew", "install", "ripgrep"
  sh "touch", "-c", "/usr/local/bin/rg"
end

desc "Install ruby"
task :ruby => ["/usr/local/bin/ruby"]
file "/usr/local/bin/ruby" => "/usr/local/bin/brew" do
  sh "brew", "install", "ruby"
  sh "touch", "-c", "/usr/local/bin/ruby"
end

ssh_files = Stow.stow(PWD.join("ssh"))
desc "Configure ssh"
task :ssh => [*ssh_files]
