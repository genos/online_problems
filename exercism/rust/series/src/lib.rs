pub fn series(digits: &str, len: usize) -> Vec<String> {
    if len == 0 {
        vec!["".to_owned(); digits.len() + 1]
    } else {
    digits
        .as_bytes()
        .windows(len)
        .filter(|c| c.len() == len)
        .filter_map(|c| std::str::from_utf8(c).ok().map(|s| s.to_owned()))
        .collect()
    }
}
