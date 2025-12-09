use itertools::Itertools;

fn parse(s: &str) -> Vec<(usize, usize)> {
    s.trim()
        .lines()
        .map(|line| {
            let (x, y) = line.split_once(',').expect("comma");
            (x.parse().expect("x"), y.parse().expect("y"))
        })
        .collect()
}

fn area((x1, y1): (usize, usize), (x2, y2): (usize, usize)) -> usize {
    (x1.abs_diff(x2) + 1) * (y1.abs_diff(y2) + 1)
}

fn part_1(coords: &[(usize, usize)]) -> usize {
    coords
        .iter()
        .tuple_combinations()
        .map(|(&a, &b)| area(a, b))
        .max()
        .unwrap_or_default()
}

fn main() {
    let input = parse(&std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", part_1(&input));
}
