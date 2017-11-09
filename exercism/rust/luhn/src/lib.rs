pub fn is_valid<S: Into<String>>(digits: S) -> bool {
    let (nums, others): (Vec<_>, Vec<_>) =
        digits.into().chars().partition(|&c| c.is_digit(10));
    if others.iter().any(|&c| !c.is_whitespace()) || nums.len() < 2 {
        false
    } else {
        nums.iter()
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
