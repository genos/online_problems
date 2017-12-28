class IsbnVerifier {

  boolean isValid(String s) {
    int sum = 0;
    char[] cs = s.replaceAll("[^0-9X]", "").toCharArray();
    if (cs.length != 10) return false;
    for (int i = 0; i < 10; i++) {
      if (cs[i] == 'X' && i != 9) return false;
      sum += "0123456789X".indexOf(cs[i]) * (10 - i);
    }
    return sum % 11 == 0;
  }

}
