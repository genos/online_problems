#[derive(Debug)]
pub enum HammingError {
    FirstLonger,
    SecondLonger,
}

pub fn hamming_distance(a: &str, b: &str) -> Result<u64, HammingError> {
    if a.len() > b.len() {
        Err(HammingError::FirstLonger)
    } else if a.len() < b.len() {
        Err(HammingError::SecondLonger)
    } else {
        Ok(a.chars()
            .zip(b.chars())
            .fold(0, |z, (x, y)| z + (if x == y { 0 } else { 1 })))
    }
}
