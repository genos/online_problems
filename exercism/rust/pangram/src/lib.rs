use std::collections::HashSet;
use std::ascii::AsciiExt;

pub fn is_pangram(s: &str) -> bool {
    26 == s.to_lowercase()
           .chars()
           .filter(|c| c.is_ascii() && c.is_alphabetic())
           .collect::<HashSet<_>>()
           .len()
}
