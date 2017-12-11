class RnaTranscription {
  String transcribe(String dnaStrand) {
    StringBuilder s = new StringBuilder();
    dnaStrand.chars().forEachOrdered(c -> s.append(lookup(c)));
    return s.toString();
  }

  char lookup(int c) {
    switch ((char) c) {
      case 'A':
        return 'U';
      case 'C':
        return 'G';
      case 'G':
        return 'C';
      case 'T':
        return 'A';
      default:
        throw new IllegalArgumentException("Invalid input");
    }
  }
}
