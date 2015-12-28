def pair_twice?(word)
  /([a-z][a-z]).*(\1)/.match(word)
end

def repeats_between?(word)
  /([a-z]).(\1)/.match(word)
end

def main
  p STDIN.readlines.map(&:downcase).count { |word|
    pair_twice?(word) && repeats_between?(word)
  }
end

main
