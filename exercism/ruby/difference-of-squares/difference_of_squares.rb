class Squares
  attr_reader :n

  def initialize(n)
    @n = n
  end

  def difference
    square_of_sum - sum_of_squares
  end

  def sum_of_squares
    @n * (@n + 1) * (2 * @n + 1) / 6
  end

  def square_of_sum
    (@n * (@n + 1) / 2) ** 2
  end
end

module BookKeeping
  VERSION = 4
end
