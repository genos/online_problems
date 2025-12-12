fn parse(s: &str) -> Vec<i16> {
    s.trim()
        .lines()
        .map(|line| {
            let (x, y) = line.split_at(1);
            (if x == "L" { -1 } else { 1 }) * y.parse::<i16>().expect("val")
        })
        .collect()
}

fn part_1(input: &[i16]) -> usize {
    input
        .iter()
        .scan(50, |a, b| {
            *a += *b;
            *a %= 100;
            Some(*a)
        })
        .filter(|&n| n == 0)
        .count()
}

fn part_2(input: &[i16]) -> usize {
    input
        .iter()
        .fold((0, 50), |(mut n, mut p), i| {
            let s = i.signum();
            for _ in 0..(s * i) {
                p += s;
                p %= 100;
                n += usize::from(p == 0);
            }
            (n, p)
        })
        .0
}

fn main() {
    let input = parse(&include_str!("../input.txt"));
    println!("{}", part_1(&input));
    println!("{}", part_2(&input));
}
