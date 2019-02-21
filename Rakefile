# frozen_string_literal: true

abort "Don't run this as root!" if Process.uid.zero?

require "open3"
require "pathname"

# Create rake tasks that emulate GNU Stow
module Stow
  extend Rake::DSL

  # Create a set of rake tasks that will link all files from the source
  # directory to the destination directory and return the identifiers
  # of those tasks
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
      into.join(dirname, source.basename)
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

def easy_description(formula, dependencies: [])
  ["Install",
   dependencies.nil? || dependencies.empty? ? nil : "and configure",
   formula.to_s].compact.join(" ")
end

def easy_install(formula, as:, dependencies: [], binary_dependencies: [])
  raise ArgumentError, "Must pass a block" unless block_given?

  desc easy_description(formula, dependencies: dependencies)
  task formula => [as, *dependencies]
  file as => binary_dependencies.dup << "/usr/local/bin/brew" do
    yield
  end
end

def brew(formula,
         as: Pathname.new("/usr/local/bin").join(formula.to_s).to_s,
         **kwargs)
  easy_install(formula, as: as, **kwargs) do
    sh "brew", "install", formula.to_s
    sh "touch", "-c", as
  end
end

def cask(formula,
         as:,
         binary_dependencies: [])
  file as => binary_dependencies.dup << "/usr/local/bin/brew" do
    sh "brew", "cask", "install", formula.to_s
    sh "touch", "-c", as
  end
end

PWD = Pathname.new(__dir__).freeze

desc "Install and configure all programs"
task default: %i[emacs fonts fzf git highlight homebrew login python
                 readline ripgrep sh ssh devenv]

emacs_files = Stow.stow(PWD.join("emacs"))
desc "Install and configure emacs"
multitask emacs: ["/Applications/Emacs.app",
                  *emacs_files]
cask "emacs", as: "/Applications/Emacs.app"

fonts_to_install = {
  "homebrew/cask-fonts/font-symbola" =>
    "#{ENV['HOME']}/Library/Fonts/Symbola_Hinted.ttf",
  "homebrew/cask-fonts/font-go-mono-nerd-font" =>
    "#{ENV['HOME']}/Library/Fonts/Go Mono Nerd Font Complete.ttf",
}.freeze
desc "Install fonts"
task fonts: [*fonts_to_install.values]
fonts_to_install.each do |formula, path| cask formula, as: path end

brew :fzf

git_files = Stow.stow(PWD.join("git"))
desc "Configure git"
multitask git: [*git_files]

brew :highlight

desc "Install homebrew"
task homebrew: ["/usr/local/bin/brew"]
file "/usr/local/bin/brew" do
  Open3.pipeline(
    ["curl", "-fsSL", "https://raw.githubusercontent.com/Homebrew/install/master/install"],
    "ruby",
  ).all?(&:zero?) or raise "fatal: Homebrew install failed"

  sh "brew", "doctor"
  sh "brew", "update"
  sh "touch", "-c", "/usr/local/bin/brew"
end

login_files = Stow.stow(PWD.join("login"))
desc "Configure login"
multitask login: [*login_files]

brew :python, as: "/usr/local/bin/python3"

readline_files = Stow.stow(PWD.join("readline"))
desc "Configure readline"
multitask readline: [*readline_files]

brew :ripgrep, as: "/usr/local/bin/rg"

sh_files = Stow.stow(PWD.join("sh"))
desc "Configure sh"
multitask sh: [*sh_files]

ssh_files = Stow.stow(PWD.join("ssh"))
desc "Configure ssh"
multitask ssh: [*ssh_files]

desc "Install bundler"
task bundle: ["/usr/local/bin/bundle"]
file "/usr/local/bin/bundle" do
  sh "sudo", "gem", "install", "bundler"
  sh "sudo", "touch", "-c", "/usr/local/bin/bundle"
  sh "sudo", "touch", "-c", "/usr/local/bin/bundler"
end

desc "Install dev environment for this repo"
task devenv: [PWD.join(".bundle")]
file PWD.join(".bundle") => "/usr/local/bin/bundle" do
  sh "bundle", "install",
     "--gemfile", PWD.join("Gemfile").to_s,
     "--path", PWD.join(".bundle").to_s

  sh "touch", "-c", PWD.join(".bundle").to_s
end
