use std::collections::HashSet;

pub fn check(s: &str) -> bool {
    let mut cs = HashSet::new();
    for c in s.to_lowercase().chars().filter(|c| c.is_alphabetic()) {
        if cs.contains(&c) {
            return false;
        } else {
            cs.insert(c);
        }
    }
    true
}
