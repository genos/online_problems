pub fn is_valid<S: Into<String>>(s: S) -> bool {
    let (digits, others): (Vec<_>, Vec<_>) = s.into().chars().partition(|&c| c.is_digit(10));
    if digits.len() < 2 || others.iter().any(|&c| !c.is_whitespace()) {
        false
    } else {
        digits
            .iter()
            .rev()
            .enumerate()
            .map(|(i, &c)| transmogrify(i, c))
            .sum::<u32>() % 10 == 0
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
