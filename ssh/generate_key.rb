#! /usr/bin/env ruby

class Object
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

def ask(question)
  $stdout.print("#{question}: ")
  $stdout.flush
  $stdin.gets.chomp.strip
end

def keygen(output_name, comment: nil, type: 'ed25519')
  args = ['ssh-keygen', '-f', output_name, '-t', type]
  if comment.present?
    args.concat(['-C', comment])
  end

  system(*args)
end

identifier = ask('Identifier')
comment = ask('Comment?')

output = File.expand_path(".ssh/id_ed25519_#{identifier}", ENV['HOME'])
keygen(output, comment: comment)

should_add = ask('Add to agent? (Y/n)').casecmp('n') != 0
if should_add
  system('ssh-add', '-K', output)
end
