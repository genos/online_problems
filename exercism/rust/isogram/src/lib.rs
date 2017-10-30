use std::collections::HashSet;

pub fn check(s: &str) -> bool {
    let xs: Vec<_> = s.to_lowercase().chars().filter(|c| c.is_alphabetic()).collect();
    let ys: HashSet<_> = xs.iter().collect();
    xs.len() == ys.len()
}
