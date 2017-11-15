use std::collections::HashMap;

pub fn word_count(s: &str) -> HashMap<String, u32> {
    let mut h = HashMap::new();
    for w in s.to_lowercase()
        .chars()
        .filter(|c| c.is_alphanumeric() || c.is_whitespace())
        .collect::<String>()
        .split_whitespace()
    {
        *h.entry(w.to_string()).or_insert(0) += 1;
    }
    h
}
