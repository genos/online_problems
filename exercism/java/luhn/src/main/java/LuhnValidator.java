import java.util.stream.IntStream;

class LuhnValidator {

  boolean isValid(String candidate) {
    String c = candidate.replace(" ", "");
    if (!c.chars().allMatch(Character::isDigit)) return false;
    int n = c.length();
    return n > 1
        && IntStream.range(0, n).map(i -> transmogrify(i, c.charAt(n - i - 1))).sum() % 10 == 0;
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
