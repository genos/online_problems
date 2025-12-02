use std::fs;

fn read(s: &str) -> Vec<(u64, u64)> {
    s.trim()
        .split(',')
        .map(|x| {
            let (y, z) = x.split_once('-').expect("sep by minus");
            (y.parse().expect("u64"), z.parse().expect("u64"))
        })
        .collect()
}

fn digits(n: u64) -> Vec<u8> {
    let mut ds = vec![];
    let mut k = n;
    while k > 0 {
        ds.push((k % 10) as u8);
        k /= 10;
    }
    ds.reverse();
    ds
}

fn solve(xs: &[(u64, u64)], pred: impl Fn(&[u8]) -> bool) -> u64 {
    xs.iter()
        .map(|(lo, hi)| (*lo..=*hi).filter(|n| pred(&digits(*n))).sum::<u64>())
        .sum()
}

fn part_1(xs: &[u8]) -> bool {
    let n = xs.len() / 2;
    xs[..n] == xs[n..]
}

fn part_2(xs: &[u8]) -> bool {
    for i in 1..=xs.len() / 2 {
        if xs.len() % i == 0
            && (0..xs.len() / i - 1).all(|j| xs[j * i..(j + 1) * i] == xs[(j + 1) * i..(j + 2) * i])
        {
            return true;
        }
    }
    false
}

fn main() {
    let input = read(&fs::read_to_string("input.txt").expect("File to read"));
    println!("{}", solve(&input, part_1));
    println!("{}", solve(&input, part_2));
}
