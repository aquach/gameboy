require 'chunky_png'

image = ChunkyPNG::Image.from_file('test.png')

color_numbers = image.height.times.map { |y|
  image.row(y).each_with_index.map { |pixel, x|
    r = (image[x, y] & 0xff00) >> 8
    (r.to_f / (256 / 4)).to_i
  }
}

puts color_numbers.map(&:inspect).join("\n")

puts color_numbers.flat_map { |row|
  high_color = row.reverse.each_with_index.map { |r, i| (r >> 1) << i }.reduce(&:|)
  low_color = row.reverse.each_with_index.map { |r, i| (r & 0x1) << i }.reduce(&:|)

  [ high_color, low_color ].map { |c| "$#{c.to_s(16).rjust(2, '0')}" }
}.join(', ')
