class Triangle {
  double _side1;
  double _side2;
  double _side3;

  Triangle(double side1, double side2, double side3) throws TriangleException {
    if ((side1 <= 0) || (side2 <= 0) || (side3 <= 0)) {
      throw new TriangleException("lengths must be positive");
    } else if ((side1 + side2 < side3) || (side1 + side3 < side2) || (side2 + side3 < side1)) {
      throw new TriangleException("fails triangle inequality");
    } else {
      _side1 = side1;
      _side2 = side2;
      _side3 = side3;
    }
  }

  boolean isEquilateral() {
    return (_side1 == _side2) && (_side2 == _side3);
  }

  boolean isIsosceles() {
    return (_side1 == _side2) || (_side1 == _side3) || (_side2 == _side3);
  }

  boolean isScalene() {
    return !(isEquilateral() || isIsosceles());
  }
}
