class Acronym {
  private String acronym;

  Acronym(String phrase) {
    StringBuilder b = new StringBuilder();
    for (String s : phrase.replaceAll("[^a-zA-Z]", " ").toUpperCase().split("\\s+")) {
      b.append(s.charAt(0));
    }
    acronym = b.toString();
  }

  String get() {
    return acronym;
  }
}
