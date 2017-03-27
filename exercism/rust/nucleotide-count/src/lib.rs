use std::collections::HashMap;
use std::iter::FromIterator;

pub fn count(c: char, s: &str) -> usize {
    *nucleotide_counts(s).get(&c).unwrap_or(&0)
}

pub fn nucleotide_counts(s: &str) -> HashMap<char, usize> {
    let mut h = HashMap::from_iter(vec![('A', 0), ('C', 0), ('G', 0), ('T', 0)]);
    for c in s.chars() {
        *h.entry(c).or_insert(0) += 1;
    }
    h
}
