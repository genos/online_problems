import java.util.*;
import java.util.stream.*;

class ProteinTranslator {

  private static int LEN = 3;
  private static String STOP = "STOP";
  private static final Map<String, String> CODON2PROTEIN;

  List<String> translate(String rnaSequence) {
    List l =
        IntStream.range(0, rnaSequence.length() / LEN)
            .map(i -> i * LEN)
            .mapToObj(i -> rnaSequence.substring(i, i + LEN))
            .map(s -> CODON2PROTEIN.get(s))
            .collect(Collectors.toList());
    int i = l.indexOf(STOP);
    if (i != -1) {
      return l.subList(0, i);
    }
    return l;
  }

  static {
    Map<String, String> c2p = new HashMap();
    String[][] codons = {
      {"AUG"},
      {"UUU", "UUC"},
      {"UUA", "UUG"},
      {"UCU", "UCC", "UCA", "UCG"},
      {"UAU", "UAC"},
      {"UGU", "UGC"},
      {"UGG"},
      {"UAA", "UAG", "UGA"},
    };
    String[] proteins = {
      "Methionine",
      "Phenylalanine",
      "Leucine",
      "Serine",
      "Tyrosine",
      "Cysteine",
      "Tryptophan",
      STOP,
    };
    assert(codons.length == proteins.length);
    for (int i = 0; i < codons.length; i++) {
      for (int j = 0; j < codons[i].length; j++) {
        assert(codons[i][j].length() == LEN);
        c2p.put(codons[i][j], proteins[i]);
      }
    }
    CODON2PROTEIN = Collections.unmodifiableMap(c2p);
  }

}
