fn transmogrify(c: char) -> char {
    if c.is_ascii() && c.is_alphabetic() {
        (26u8 - (c as u8 - b'a' + 1) + b'a') as char
    } else {
        c
    }
}

pub fn encode(s: &str) -> String {
    s.to_ascii_lowercase()
        .chars()
        .filter(|c| c.is_ascii() && c.is_alphanumeric())
        .map(transmogrify)
        .collect::<Vec<char>>()
        .chunks(5)
        .map(|k| k.iter().collect())
        .collect::<Vec<String>>()
        .join(" ")
}

pub fn decode(s: &str) -> String {
    s.chars()
        .filter(|c| c.is_alphanumeric())
        .map(transmogrify)
        .collect()
}
