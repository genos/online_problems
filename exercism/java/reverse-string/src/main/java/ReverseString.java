class ReverseString {
  public String reverse(String input) {
    // return new StringBuilder(input).reverse().toString();
    return input
        .chars()
        .mapToObj((int i) -> Character.toString((char) i))
        .reduce("", (String acc, String c) -> c + acc);
  }
}
