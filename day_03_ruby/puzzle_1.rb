input = File.read('input.txt').strip
regex = /mul\((\d{1,3}),(\d{1,3})\)/

total_sum = 0
input.scan(regex) do |x, y|
  total_sum += x.to_i * y.to_i
end

puts total_sum
