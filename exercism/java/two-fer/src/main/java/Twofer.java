public class Twofer {
  public String twofer(String name) {
      String n = name;
      if (n == null) { n = "you"; }
      return "One for " + n + ", one for me.";
  }
}
