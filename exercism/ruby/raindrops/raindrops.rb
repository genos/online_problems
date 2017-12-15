module Raindrops
  SOUNDS = {3 => :Pling, 5 => :Plang, 7 => :Plong}
  def self.convert(i)
    s = SOUNDS.map { |n, p| p if (i % n).zero? }.join
    s.empty? ? i.to_s : s
  end
end

module BookKeeping
  VERSION = 3
end
