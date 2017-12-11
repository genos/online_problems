import java.time.LocalDate;
import java.time.LocalDateTime;

class Gigasecond {
  LocalDateTime _birth;

  Gigasecond(LocalDate birthDate) {
    _birth = birthDate.atStartOfDay();
  }

  Gigasecond(LocalDateTime birthDateTime) {
    _birth = birthDateTime;
  }

  LocalDateTime getDate() {
    return _birth.plusSeconds((long) 1e9);
  }
}
