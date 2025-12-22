fn parse(s: &str) -> Vec<Vec<u64>> {
    s.trim()
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| u64::from(c.to_digit(10).expect("digit")))
                .collect()
        })
        .collect()
}

// help from https://www.reddit.com/r/adventofcode/comments/1pd0cp6/2025_day_03_cli_visualization/
fn max_joltage(n: usize, bank: &[u64]) -> u64 {
    let (mut joltage, mut i, mut j) = (0, 0, bank.len() - n + 1);
    for _ in 0..n {
        let (k, &max) = bank[i..j]
            .iter()
            .enumerate()
            .rev()
            .max_by_key(|(_k, b)| *b)
            .expect("argmax");
        joltage = 10 * joltage + max;
        i += k + 1;
        j += 1;
    }
    joltage
}

fn part_1(banks: &[Vec<u64>]) -> u64 {
    banks.iter().map(|b| max_joltage(2, b)).sum()
}

fn part_2(banks: &[Vec<u64>]) -> u64 {
    banks.iter().map(|b| max_joltage(12, b)).sum()
}

fn main() {
    let input = parse(
        &std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/input.txt")).expect("file"),
    );
    println!("{}", part_1(&input));
    println!("{}", part_2(&input));
}
