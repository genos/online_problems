import java.util.*;

class HandshakeCalculator {

  static int REVERSE = 0b10000;

  static boolean check(int number, int base) {
    return base == (number & base);
  }

  List<Signal> calculateHandshake(int number) {
    List result = new ArrayList();
    for (Signal signal: Signal.values()) {
      if (check(number, 1 << signal.ordinal())) {
        result.add(signal);
      }
    }
    if (check(number, REVERSE)) {
      Collections.reverse(result);
    }
    return result;
  }

}
