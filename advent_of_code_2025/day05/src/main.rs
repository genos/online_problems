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

fn part2_(ranges: &[(u64, u64)]) -> u64 {
    let mut xs = BTreeSet::new();
    for &r in ranges {
        let overlaps = xs.extract_if(.., |&(a, b)| !(a > r.1 || b < r.0));
        let s = overlaps.fold(r, |(a, b), (x, y)| (a.min(x), b.max(y)));
        xs.insert(s);
    }
    xs.iter().map(|&(a, b)| b - a + 1).sum()
}

fn main() {
    let (ranges, ingredients) = parse(&std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", part1(&ranges, &ingredients));
    println!("{}", part2_(&ranges));
}
