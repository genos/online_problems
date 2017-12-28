import java.util.*;
import java.util.stream.*;

class LuhnValidator {

  boolean isValid(String candidate) {
    List<Character> s = new ArrayList();
    for (int i = 0; i < candidate.length(); i++) {
      char c = candidate.charAt(i);
      if (!(Character.isDigit(c) || Character.isWhitespace(c))) {
        return false;
      } else if (Character.isDigit(c)) {
        s.add(c);
      }
    }
    int n = s.size();
    return n > 1
        && IntStream.range(0, n).map(i -> transmogrify(i, s.get(n - i - 1))).sum() % 10 == 0;
  }

  int transmogrify(int i, char c) {
    int x = Character.digit(c, 10);
    if (0 == (i & 1)) {
      return x;
    } else if (x > 4) {
      return x + x - 9;
    } else {
      return x + x;
    }
  }
}
