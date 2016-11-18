use std::collections::HashMap;

pub fn score(s: &str) -> u64 {
    let t = {
        let mut h = HashMap::new();
        for (x, i) in vec![("AEIOULNRST", 1),
                           ("DG", 2),
                           ("BCMP", 3),
                           ("FHVMY", 4),
                           ("K", 5),
                           ("JX", 8),
                           ("QZ", 10)] {
            for c in x.chars() {
                h.insert(c, i);
            }
        }
        h
    };
    s.to_uppercase().chars().map(|c| *t.get(&c).unwrap_or(&0)).sum()
}
