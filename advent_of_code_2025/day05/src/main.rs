use rayon::prelude::*;
use std::collections::BTreeSet;

fn parse(s: &str) -> (Vec<(u64, u64)>, Vec<u64>) {
    let (first, second) = s.split_once("\n\n").expect("spacing");
    let ranges = first
        .trim()
        .lines()
        .map(|line| {
            let (a, b) = line.trim().split_once('-').expect("hyphen");
            (a.parse().expect("a"), b.parse().expect("b"))
        })
        .collect();
    let ingredients = second
        .trim()
        .lines()
        .map(|line| line.trim().parse().expect("ingredient"))
        .collect();
    (ranges, ingredients)
}

fn part1(ranges: &[(u64, u64)], ingredients: &[u64]) -> usize {
    ingredients
        .iter()
        .filter(|&&i| ranges.iter().any(|&(a, b)| a <= i && i <= b))
        .count()
}

fn part2(ranges: &[(u64, u64)]) -> usize {
    ranges
        .into_par_iter()
        .fold_chunks(5, BTreeSet::new, |mut acc, &(a, b)| {
            for n in a..=b {
                acc.insert(n);
            }
            acc
        })
        .fold(BTreeSet::new, |mut acc, xs| {
            for n in xs {
                acc.insert(n);
            }
            acc
        })
        .count()
}

fn main() {
    let (ranges, ingredients) = parse(&std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", part1(&ranges, &ingredients));
    //println!("{}", part2(&ranges));
}
