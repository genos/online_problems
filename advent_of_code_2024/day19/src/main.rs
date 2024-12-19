use fxhash::FxHashSet;
use rayon::prelude::*;
use std::fs;

fn parse(input: &str) -> (FxHashSet<&[u8]>, Vec<&[u8]>) {
    let mut it = input.lines();
    let av = it
        .next()
        .expect("bad input")
        .split(", ")
        .map(str::as_bytes)
        .collect();
    (av, it.skip(1).map(str::as_bytes).collect())
}

// dynamic programming: word break, and count
fn wbc(available: &FxHashSet<&[u8]>, d: &[u8]) -> (u64, u64) {
    let mut dp = vec![0; d.len() + 1];
    dp[0] = 1;
    for i in 0..=d.len() {
        for j in 0..i {
            if available.contains(&d[j..i]) {
                dp[i] += dp[j];
            }
        }
    }
    let n = dp[d.len()];
    (u64::from(n > 0), n)
}

fn solve(available: &FxHashSet<&[u8]>, designs: &[&[u8]]) -> (u64, u64) {
    designs
        .par_iter()
        .map(|d| wbc(available, d))
        .reduce(|| (0, 0), |(a0, a1), (b0, b1)| (a0 + b0, a1 + b1))
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Can't read file");
    let (available, designs) = parse(&input);
    let (p1, p2) = solve(&available, &designs);
    println!("{p1}");
    println!("{p2}");
}
