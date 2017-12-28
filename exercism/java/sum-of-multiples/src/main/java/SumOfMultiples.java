import java.util.stream.*;

class SumOfMultiples {

  int _s;

  SumOfMultiples(int number, int[] set) {
    _s =
        IntStream.range(0, number).filter(i -> IntStream.of(set).anyMatch(n -> 0 == (i % n))).sum();
  }

  int getSum() {
    return _s;
  }

}
