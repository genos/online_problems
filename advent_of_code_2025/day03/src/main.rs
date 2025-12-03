fn digits(s: &str) -> Vec<Vec<u64>> {
    s.trim()
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| u64::from(c.to_digit(10).expect("digit")))
                .collect()
        })
        .collect()
}

fn argmax(bank: &[u64]) -> (usize, u64) {
    let mut i = 0;
    let mut m = bank[i];
    for (j, b) in bank.iter().enumerate() {
        if m < *b {
            m = *b;
            i = j;
        }
    }
    (i, m)
}

fn solve_one(n: usize, bank: &[u64]) -> u64 {
    let mut b = 0;
    let mut i = 0;
    let mut len = 0;
    let mut j = bank.len() - n + 1;
    while len < n {
        len += 1;
        let (k, m) = argmax(&bank[i..j]);
        b = 10 * b + m;
        i += k + 1;
        j += 1;
    }
    b
}

fn part_1(banks: &[Vec<u64>]) -> u64 {
    banks.iter().map(|b| solve_one(2, b)).sum()
}

fn part_2(banks: &[Vec<u64>]) -> u64 {
    banks.iter().map(|b| solve_one(12, b)).sum()
}

fn main() {
    let input = digits(&std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", part_1(&input));
    println!("{}", part_2(&input));
}
