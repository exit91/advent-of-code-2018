lines = readlines(open("03-input.txt"))
re_claim = r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
function parse_line(line)
  mline = match(re_claim, line)
  id = parse(Int, mline[1])
  x_min = parse(Int, mline[2]) + 1
  y_min = parse(Int, mline[3]) + 1
  width = parse(Int, mline[4])
  height = parse(Int, mline[5])
  x_max = x_min + width - 1
  y_max = y_min + height - 1
  Dict(:id => id, :x_min => x_min, :y_min => y_min, :x_max => x_max, :y_max => y_max)
end
claims = parse_line.(lines)

m = zeros(maximum(map(d -> d[:x_max], claims)),
          maximum(map(d -> d[:y_max], claims)))
for claim in claims
  m[claim[:x_min]:claim[:x_max], claim[:y_min]:claim[:y_max]] .+= 1
end

println(length(findall(m .> 1)))
