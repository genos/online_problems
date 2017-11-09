pub fn is_valid<S: Into<String>>(digits: S) -> bool {
    let s: String = digits.into();
    if s.chars().all(|c| c.is_digit(10) || c.is_whitespace()) {
        let cs = s.chars().filter(|c| c.is_digit(10)).collect::<Vec<_>>();
        cs.len() > 1
            && cs.into_iter()
                .rev()
                .enumerate()
                .map(|(i, c)| transmogrify(i, c))
                .sum::<u32>() % 10 == 0
    } else {
        false
    }
}

fn transmogrify(i: usize, c: char) -> u32 {
    let x = c.to_digit(10).unwrap();
    if i & 1 == 0 {
        x
    } else if x > 4 {
        x + x - 9
    } else {
        x + x
    }
}
