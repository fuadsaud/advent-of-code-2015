p STDIN.readlines
  .map { |line| line.split('x').map(&:to_i) }
  .map { |l, w, h| (2 * l * w) + (2 * w * h) + (2 * h * l) + [l * w, w * h, h * l].min }
  .reduce(&:+)
