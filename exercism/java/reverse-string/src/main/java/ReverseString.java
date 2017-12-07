class ReverseString {
  public String reverse(String input) {
    // return new StringBuilder(input).reverse().toString();
    // return input
    //     .chars()
    //     .mapToObj((int i) -> Character.toString((char) i))
    //     .reduce("", (String acc, String c) -> c + acc);
    return input
        .chars()
        .collect(StringBuilder::new, (sb, i) -> sb.insert(0, (char) i), StringBuilder::append)
        .toString();
  }
}
