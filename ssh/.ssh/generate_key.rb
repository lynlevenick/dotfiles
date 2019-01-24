#! /usr/bin/env ruby
# frozen_string_literal: true

# Provide ActiveSupport-style Object#blank? and Object#present? methods
module Presence
  refine Object do
    def blank?
      if respond_to?(:empty?)
        empty?
      else
        nil?
      end
    end

    def present?
      !blank?
    end
  end
end

using Presence

def ask(question)
  $stdout.print("#{question}: ")
  $stdout.flush
  $stdin.gets.chomp.strip
end

def keygen(output_name, comment: nil, type: "ed25519")
  args = ["ssh-keygen", "-f", output_name, "-t", type]
  args.concat(["-C", comment]) if comment.present?

  system({ "SSH_USE_STRONG_RNG" => "1" }, *args)
end

identifier = ask("Identifier")
comment = ask("Comment?")

output = File.expand_path(".ssh/id_ed25519_#{identifier}", ENV["HOME"])
keygen(output, comment: comment)

ask("Add to agent? (Y/n)").casecmp("n") != 0 and system("ssh-add", "-K", output)
