pub fn series(digits: &str, len: usize) -> Vec<String> {
    if len == 0 {
        vec!["".to_owned(); digits.len() + 1]
    } else {
        digits
            .chars()
            .collect::<Vec<_>>()
            .windows(len)
            .filter(|c| c.len() == len)
            .map(|c| c.iter().collect())
            .collect()
    }
}
