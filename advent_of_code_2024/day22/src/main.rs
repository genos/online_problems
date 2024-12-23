use itertools::Itertools;
use rayon::prelude::*;
use std::{collections::HashMap, fs, iter::from_fn};

fn next_secret(s: i64) -> i64 {
    let mut t = s;
    t = (t ^ (t << 6)) % 16_777_216;
    t = (t ^ (t >> 5)) % 16_777_216;
    t = (t ^ (t << 11)) % 16_777_216;
    t
}

fn secret_stream(s: i64) -> impl Iterator<Item = i64> {
    let mut s = s;
    from_fn(move || {
        let out = s;
        s = next_secret(s);
        Some(out)
    })
}

fn part1(ss: &[i64]) -> i64 {
    ss.par_iter()
        .map(|&s| secret_stream(s).nth(2000).unwrap_or_default())
        .sum()
}

fn price(s: i64) -> i64 {
    s % 10
}

fn price_map(s: i64) -> HashMap<(i64, i64, i64, i64), i64> {
    let mut out = HashMap::new();
    for (a, b, c, d, e) in secret_stream(s).map(price).take(2000).tuple_windows() {
        let k = (b - a, c - b, d - c, e - d);
        if !out.contains_key(&k) {
            out.insert(k, e);
        }
    }
    out
}

fn merge_maps(
    a: HashMap<(i64, i64, i64, i64), i64>,
    b: HashMap<(i64, i64, i64, i64), i64>,
) -> HashMap<(i64, i64, i64, i64), i64> {
    let mut out = a.clone();
    for (k, v) in b {
        *out.entry(k).or_default() += v;
    }
    out
}

fn part2(ss: &[i64]) -> i64 {
    ss.into_par_iter()
        .map(|&s| price_map(s))
        .reduce(HashMap::new, merge_maps)
        .into_values()
        .max()
        .unwrap_or_default()
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Can't read file");
    let ss = input
        .lines()
        .map(|l| l.parse().expect("Can't parse line"))
        .collect::<Vec<i64>>();
    println!("{}", part1(&ss));
    println!("{}", part2(&ss));
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn secret_example() {
        assert_eq!(
            secret_stream(123).take(11).collect::<Vec<_>>(),
            [
                123, 15_887_950, 16_495_136, 527_345, 704_524, 1_553_684, 12_683_156, 11_100_544,
                12_249_484, 7_753_432, 5_908_254
            ]
        );
    }
    #[test]
    fn price_example() {
        assert_eq!(
            secret_stream(123).take(10).map(price).collect::<Vec<_>>(),
            [3, 0, 6, 5, 4, 4, 6, 4, 4, 2]
        );
    }
}
