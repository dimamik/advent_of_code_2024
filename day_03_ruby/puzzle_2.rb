input = File.read('input.txt').strip
regex = /mul\((\d{1,3}),(\d{1,3})\)|(do\(\)|don't\(\))/

counting = true
sum = 0
input.scan(regex) do |mul_x, mul_y, do_dont|
  if mul_x && mul_y && counting
    sum += mul_x.to_i * mul_y.to_i
  elsif do_dont == "do()"
    counting = true
  elsif  do_dont == "don't()"
    counting = false
  end
end

puts sum