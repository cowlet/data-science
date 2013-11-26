#!/usr/bin/env ruby

def max_subset(sets, n)
  sets.combination(n).max_by {|s| covered_features(s).size }
end

def covered_features(sets)
  sets.flatten.uniq
end

lines = File.readlines(ARGV[0]).map &:chomp
sets = lines.map {|s| s.gsub(/"/, '').split }
puts "Total features: #{sets.size}"

uniques, correlated = sets.partition {|s| s.size == 1 }
puts "Unique features: #{uniques.size}"
puts "Correlated features: #{correlated.size}"

features = covered_features(correlated)

n = 0
subset = []
while covered_features(subset).uniq.sort != features.uniq.sort
  n += 1
  subset = max_subset(correlated, n)
end

puts "Smallest number of correlated features covering all features: #{n}"

# Take only the first element, as this is the source of the subset
result = (uniques + subset).map {|s| s[0] }

puts "#{result.size} total features required:\n\n"
puts result.inspect
