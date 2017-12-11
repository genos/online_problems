class RaindropConverter {
  String convert(int number) {
    StringBuilder b = new StringBuilder();
    if (number % 3 == 0) {
      b.append("Pling");
    }
    if (number % 5 == 0) {
      b.append("Plang");
    }
    if (number % 7 == 0) {
      b.append("Plong");
    }
    if (b.length() == 0) {
      b.append(String.valueOf(number));
    }
    return b.toString();
  }
}
