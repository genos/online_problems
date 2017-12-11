class DifferenceOfSquaresCalculator {
  int computeSquareOfSumTo(int n) {
    int m = n * (n + 1) / 2;
    return m * m;
  }

  int computeSumOfSquaresTo(int n) {
    return n * (n + 1) * (2 * n + 1) / 6;
  }

  int computeDifferenceOfSquares(int n) {
    return computeSquareOfSumTo(n) - computeSumOfSquaresTo(n);
  }
}
