fn parse(s: &str) -> (Vec<(u64, u64)>, Vec<u64>) {
    let (rs, is) = s.trim().split_once("\n\n").expect("spacing");
    let ranges = rs
        .lines()
        .map(|r| {
            let (a, b) = r.split_once('-').expect("hyphen");
            (a.parse().expect("a"), b.parse().expect("b"))
        })
        .collect();
    let ingredients = is.lines().map(|i| i.parse().expect("ingredient")).collect();
    (ranges, ingredients)
}

fn part_1(ranges: &[(u64, u64)], ingredients: &[u64]) -> usize {
    ingredients
        .iter()
        .filter(|&&i| ranges.iter().any(|&(a, b)| a <= i && i <= b))
        .count()
}

fn part_2(ranges: &[(u64, u64)]) -> u64 {
    let mut merged = Vec::with_capacity(ranges.len());
    for &r in ranges {
        let s = merged
            .extract_if(.., |&mut (a, b)| !(a > r.1 || b < r.0))
            .fold(r, |(a, b), (x, y)| (a.min(x), b.max(y)));
        merged.push(s);
    }
    merged.iter().map(|&(a, b)| b - a + 1).sum()
}

fn main() {
    let (ranges, ingredients) = parse(&std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", part_1(&ranges, &ingredients));
    println!("{}", part_2(&ranges));
}
