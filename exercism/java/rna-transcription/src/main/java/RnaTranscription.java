class RnaTranscription {
  String transcribe(String dnaStrand) {
    StringBuilder s = new StringBuilder();
    return dnaStrand
        .chars()
        .mapToObj((i) -> lookup(i))
        .collect(StringBuilder::new, (b, c) -> b.append(c), StringBuilder::append)
        .toString();
  }

  char lookup(int c) {
    switch ((char) c) {
      case 'A': return 'U';
      case 'C': return 'G';
      case 'G': return 'C';
      case 'T': return 'A';
      default: throw new IllegalArgumentException("Invalid input");
    }
  }
}
