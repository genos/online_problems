public class PangramChecker {
  public boolean isPangram(String input) {
    return input
            .chars()
            .map(Character::toLowerCase)
            .filter(c -> 'a' <= c && c <= 'z')
            .distinct()
            .toArray()
            .length
        == 26;
  }
}
