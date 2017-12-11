class SpaceAge {
  double _age;

  SpaceAge(double seconds) {
    _age = seconds;
  }

  double getSeconds() {
    return _age;
  }

  double onEarth() {
    return _age / 31557600;
  }

  double onMercury() {
    return onEarth() / 0.2408467;
  }

  double onVenus() {
    return onEarth() / 0.61519726;
  }

  double onMars() {
    return onEarth() / 1.8808158;
  }

  double onJupiter() {
    return onEarth() / 11.862615;
  }

  double onSaturn() {
    return onEarth() / 29.447498;
  }

  double onUranus() {
    return onEarth() / 84.016846;
  }

  double onNeptune() {
    return onEarth() / 164.79132;
  }
}
