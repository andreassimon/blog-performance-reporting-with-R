#!/usr/bin/env ruby
name = ARGV[0]
quiet = true

$stdin.each_line do |line|
  if !quiet && line =~ / endregion #{name}/
    quiet = true
    if ellipsis = (line.match(/ endregion #{name}(.+$)$/))
      puts ellipsis[1]
    end
  end
  puts line unless quiet
  if quiet && line =~ / region #{name}/
    quiet = false
  end
end
