module BookKeeping
  VERSION = 3
end

# My submission for exercism.io's "Hamming" exercise.
class Hamming
  def self.compute(x, y)
    raise(ArgumentError, 'Different lengths') unless x.length == y.length
    x.each_char.zip(y.each_char).map { |a, b| a == b ? 0 : 1 }.sum
  end
end
