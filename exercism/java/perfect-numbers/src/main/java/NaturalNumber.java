import java.util.stream.LongStream;

class NaturalNumber {

  long _n;

  NaturalNumber(int n) {
    if (n <= 0) {
      throw new IllegalArgumentException("You must supply a natural number (positive integer)");
    }
    _n = n;
  }

  Classification getClassification() {
    long a = LongStream.range(1, _n).filter(i -> _n % i == 0).sum();
    if (a == _n) {
      return Classification.PERFECT;
    } else if (a > _n) {
      return Classification.ABUNDANT;
    } else {
      return Classification.DEFICIENT;
    }
  }
}
