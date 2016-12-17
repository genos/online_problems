use std::collections::HashSet;

pub fn is_pangram(s: &str) -> bool {
    let mut h = HashSet::new();
    for c in s.to_lowercase().chars().filter(|c| *c >= 'a' && *c <= 'z') {
        h.insert(c);
    }
    return 26 == h.len();
}
