import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

class ArmstrongNumbers {

  int pow(int n, int k) {
    if (k == 0) {
      return 1;
    } else if (k == 1) {
      return n;
    } else {
      int p = pow(n * n, k / 2);
      return ((k & 1) == 1) ? n * p : p;
    }
  }

  List digits(int number) {
    if (number < 0) {
      return digits(-number);
    } else if (number == 0) {
      return Collections.singletonList(0);
    } else {
      List result = new LinkedList();
      while (number > 0) {
        result.add(number % 10);
        number = number / 10;
      }
      Collections.reverse(result);
      return result;
    }
  }

  boolean isArmstrongNumber(int numberToCheck) {
    List digits = digits(numberToCheck);
    return numberToCheck
        == digits.parallelStream().mapToInt(n -> pow((int) n, digits.size())).sum();
  }
}
