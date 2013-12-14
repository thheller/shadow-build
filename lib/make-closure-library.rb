#!/usr/bin/env ruby

require 'fileutils'
require 'set'

if File.exists?("closure-library")
  system("cd closure-library; git pull origin master")
else
  system("git clone https://code.google.com/p/closure-library/")
end

FileUtils.mkdir_p("js-closure")


IGNORE = Set.new(["closure-library/third_party/closure/goog/base.js",
                  "closure-library/third_party/closure/goog/deps.js",
                  "closure-library/closure/goog/deps.js"])

Dir["closure-library/**/*.js"].each do |it|
  next if IGNORE.include?(it)

  if it =~ /^(.*)\/goog\//
    target = "js-closure#{it.gsub($1, '')}"
    FileUtils.mkdir_p(File.dirname(target))

    FileUtils.cp(it, target)
    p target
  end
end

system("python closure-library/closure/bin/build/depswriter.py --root=js-closure/goog --output_file=js-closure/goog/deps.js")
