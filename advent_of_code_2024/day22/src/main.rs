use itertools::Itertools;
use rayon::prelude::*;
use std::{collections::HashMap, fs, iter::from_fn};

fn secret_stream(s: u32) -> impl Iterator<Item = u32> {
    let mut s = s;
    from_fn(move || {
        let out = s;
        s = (s ^ (s << 6)) & ((1 << 24) - 1);
        s = (s ^ (s >> 5)) & ((1 << 24) - 1);
        s = (s ^ (s << 11)) & ((1 << 24) - 1);
        Some(out)
    })
}

fn part1(ss: &[u32]) -> u64 {
    ss.par_iter()
        .map(|&s| u64::from(secret_stream(s).nth(2000).unwrap_or_default()))
        .sum()
}

fn price_map(s: u32) -> HashMap<(i8, i8, i8, i8), u16> {
    let mut out = HashMap::new();
    for (a, b, c, d, e) in secret_stream(s)
        .map(|t| (t % 10) as i8)
        .take(2000)
        .tuple_windows()
    {
        #[allow(clippy::cast_sign_loss)]
        out.entry((b - a, c - b, d - c, e - d)).or_insert(e as u16);
    }
    out
}

fn part2(ss: &[u32]) -> u16 {
    ss.into_par_iter()
        .map(|&s| price_map(s))
        .reduce(HashMap::new, |a, b| {
            let mut out = a.clone();
            for (k, v) in b {
                *out.entry(k).or_default() += v;
            }
            out
        })
        .into_values()
        .max()
        .unwrap_or_default()
}

fn main() {
    let ss = fs::read_to_string("input.txt")
        .expect("Can't read file")
        .lines()
        .map(|l| l.parse().expect("Can't parse line"))
        .collect::<Vec<_>>();
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
}
