#!/usr/bin/env ruby

require 'shellwords'

filename = ARGV[0]

def special_method?(name)
  special_methods = %I{nil? send object_id instance_eval}
  name =~ /^__/ || special_methods.include?(name)
end

# A script type, such as shell, Python, Perl, etc.
class ScriptType
  attr_reader :name

  class <<
    @instances = {}
  end

  def initialize(name)
    @name = name
  end

  def template
    "#!#{path}\n"
  end

  def path
    fail "forgot to define #path for #{name}"
  end

  def match_extension?
    fail "forgot to define #match_extension?() for #{name}"
  end

  def correct_shebang?(given_path)
    given_path == path
  end

  def self.add(name, &block)
    # Create a new sub-type, evaluate the block
    cls = Class.new(ScriptType)
    cls.module_eval(&block)

    instance = cls.new(name)
    @instances[instance.name] = instance
    instance
  end

  def self.match(filename)
    noisnetxe, rest = filename.reverse.split('.', 2)
    if rest.nil?
      self[:shell]
    else
      extension = noisnetxe.reverse
      type_by_extension(extension)
    end
  end

  def self.type_by_extension(extension)
    types.each_value do |type|
      return type if type.match_extension?(extension)
    end
    fail "Unknown extension: .#{extension}"
  end

  def self.[](name)
    type = types[name]
    fail "Undefined script type: #{name}" if type.nil?
    type
  end

  def self.types
    @instances
  end
end

ScriptType.add :shell do
  def match_extension?(extension)
    extension == 'sh'
  end

  def path
    '/bin/sh'
  end
end

ScriptType.add :python do
  def match_extension?(extension)
    extension == 'py'
  end

  def path
    '/usr/bin/env python'
  end
end

ScriptType.add :perl do
  def match_extension?(extension)
    extension == 'pl'
  end

  def path
    '/usr/bin/env perl'
  end

  def template
    <<-SCRIPT
#!#{path}

use strict;
    SCRIPT
  end
end

ScriptType.add :erlang do
  def match_extension?(extension)
    extension == 'erl'
  end

  def path
    '/usr/bin/env escript'
  end
end

ScriptType.add :ruby do
  def match_extension?(extension)
    extension == 'rb'
  end

  def path
    '/usr/bin/env ruby'
  end
end

# An editor, derived from context.
class Editor
  @command = nil
  @args = []
  @subclasses = {}

  class << self
    attr_reader :args
  end

  def edit(filename)
    pid = Process.spawn(make_arg_string(filename))
    Process.wait(pid)
  end

  def self.inherited(cls)
    name = cls.name.downcase
    @subclasses[name] = cls.new
  end

  def self.command(*args)
    if args.empty?
      @command
    else
      name, *args = args
      @command = name
      @args = args
    end
  end

  def self.from_environment
    name = ENV['VISUAL'] || ENV['EDITOR']
    if name.nil?
      fail 'Cannot determine your editor: both EDITOR and VISUAL are undefined'
    else
      Editor.from_command(name)
    end
  end

  def self.from_command(name)
    fail "Unknown editor: #{name}" unless @subclasses.include?(name)
    @subclasses[name]
  end

  private

  def make_arg_string(filename)
    Shellwords.join(self.class.command_with_args(filename))
  end

  def self.command_with_args(filename)
    [command] + args.map do |arg|
      if arg == :filename
        filename
      else
        arg
      end
    end
  end
end

# Definition for Vim
class Vim < Editor
  command 'vim', '+norm G', :filename
end

# Definition for Nano
class Nano < Editor
  command 'nano', :filename
end

# Definition for Emacs
class Emacs < Editor
  command 'emacs', :filename
end

# The script to create or modify.
class Script
  attr_reader :filename, :script_type

  def self.new(filename, &block)
    obj = super(filename)
    obj.instance_eval(&block) if block_given?
    obj
  end

  def initialize(filename)
    fail 'No filename given' if filename.nil?
    @filename = filename
    @script_type = ScriptType.match(filename)
    @file = nil
  end

  def ensure_exists
    return if File.exist?(filename)
    open_for_appending!
  end

  def set_executable
    open_for_appending
    current_mode = File.stat(filename).mode
    @file.chmod(current_mode | 0111)
  end

  def inject_template_from_name
    @file.write script_type.template
    @file.fsync
  end

  def edit
    editor = Editor.from_environment
    editor.edit(filename)
  end

  def correct_shebang?
    open_for_appending
    @file.seek(0, :SET)

    # File has no contents
    return false if @file.eof?

    # Read the first line; check for shebang.
    first_line = @file.gets("\n")
    return false unless first_line.start_with?('#!')

    path = first_line[2..-1].chomp
    script_type.correct_shebang?(path)
  end

  private

  def open_for_appending
    open_for_appending! unless @file.is_a?(File) && !@file.closed?
  end

  def open_for_appending!
    @file = File.new(filename, 'a+t:UTF-8')
    self
  end
end

Script.new filename do
  ensure_exists
  set_executable
  inject_template_from_name unless correct_shebang?
  edit
end
