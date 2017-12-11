import java.util.*;

class Scrabble {
  private static final Map<Character, Integer> TABLE;
  private int score;

  Scrabble(String word) {
    score =
        word.chars().map(Character::toUpperCase).map(c -> TABLE.getOrDefault((char) c, 0)).sum();
  }

  int getScore() {
    return score;
  }

  static {
    Map<Character, Integer> table = new HashMap();
    int[] scores = {1, 2, 3, 4, 5, 8, 10};
    char[][] chars = {
      {'A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T'},
      {'D', 'G'},
      {'B', 'C', 'M', 'P'},
      {'F', 'H', 'V', 'W', 'Y'},
      {'K'},
      {'J', 'X'},
      {'Q', 'Z'}
    };
    for (int i = 0; i < scores.length; i++) {
      for (int j = 0; j < chars[i].length; j++) {
        table.put(chars[i][j], scores[i]);
      }
    }
    TABLE = Collections.unmodifiableMap(table);
  }
}
