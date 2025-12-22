fn parse(s: &str) -> Vec<(u64, u64)> {
    s.trim()
        .split(',')
        .map(|x| {
            let (y, z) = x.split_once('-').expect("sep by minus");
            (y.parse().expect("u64"), z.parse().expect("u64"))
        })
        .collect()
}

fn rev_digits(mut n: u64) -> Vec<u8> {
    let mut ds = vec![];
    while n > 0 {
        ds.push((n % 10) as u8);
        n /= 10;
    }
    ds
}

fn solve(xs: &[(u64, u64)], pred: impl Fn(&[u8]) -> bool) -> u64 {
    xs.iter()
        .map(|(lo, hi)| (*lo..=*hi).filter(|n| pred(&rev_digits(*n))).sum::<u64>())
        .sum()
}

fn part_1(xs: &[u8]) -> bool {
    let n = xs.len() / 2;
    xs[..n] == xs[n..]
}

fn part_2(xs: &[u8]) -> bool {
    (1..=xs.len() / 2).any(|i| {
        xs.len().is_multiple_of(i)
            && (0..xs.len() / i - 1).all(|j| xs[j * i..(j + 1) * i] == xs[(j + 1) * i..(j + 2) * i])
    })
}

fn main() {
    let input = parse(
        &std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/input.txt")).expect("file"),
    );
    for p in [part_1, part_2] {
        println!("{}", solve(&input, p));
    }
}
