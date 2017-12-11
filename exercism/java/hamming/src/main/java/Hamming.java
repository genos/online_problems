import java.util.stream.IntStream;

public class Hamming {
  int _distance;

  Hamming(String leftStrand, String rightStrand) {
    if (leftStrand.length() != rightStrand.length()) {
      throw new IllegalArgumentException("leftStrand and rightStrand must be of equal length.");
    }
    _distance =
        IntStream.range(0, leftStrand.length())
            .map(i -> leftStrand.charAt(i) == rightStrand.charAt(i) ? 0 : 1)
            .sum();
  }

  int getHammingDistance() {
    return _distance;
  }
}
