use std::fs::File;
use std::io::prelude::*;

fn read_input() -> String {
    let mut f = File::open("input.txt").expect("'input.txt' not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("unable to read 'input.txt'");
    contents
}

fn parse_input(s: &str) -> Vec<u32> {
    s.chars().filter_map(|c| c.to_digit(10)).collect()
}

#[derive(PartialEq)]
enum Step {
    One,
    HalfOfLen,
}

fn sum_matches(ns: &[u32], step: &Step) -> u32 {
    let len: usize = ns.len();
    let j: usize = match *step {
        Step::One => 1,
        Step::HalfOfLen => len / 2,
    };
    (0..len)
        .filter(|&i| ns[i] == ns[(i + j) % len])
        .map(|i| ns[i])
        .sum()
}
fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", sum_matches(&input, &Step::One));
    println!("Part 2: {}", sum_matches(&input, &Step::HalfOfLen));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_input() {
        assert_eq!(parse_input("1122"), &[1, 1, 2, 2]);
        assert_eq!(parse_input("1111"), &[1, 1, 1, 1]);
        assert_eq!(parse_input("1234"), &[1, 2, 3, 4]);
        assert_eq!(parse_input("91212129"), &[9, 1, 2, 1, 2, 1, 2, 9]);
    }

    #[test]
    fn test_sum_matches_one() {
        assert_eq!(sum_matches(&[1, 1, 2, 2], &Step::One), 3);
        assert_eq!(sum_matches(&[1, 1, 1, 1], &Step::One), 4);
        assert_eq!(sum_matches(&[1, 2, 3, 4], &Step::One), 0);
        assert_eq!(sum_matches(&[9, 1, 2, 1, 2, 1, 2, 9], &Step::One), 9);
    }

    #[test]
    fn test_sum_matches_half() {
        assert_eq!(sum_matches(&[1, 2, 1, 2], &Step::HalfOfLen), 6);
        assert_eq!(sum_matches(&[1, 2, 2, 1], &Step::HalfOfLen), 0);
        assert_eq!(sum_matches(&[1, 2, 3, 4, 2, 5], &Step::HalfOfLen), 4);
        assert_eq!(sum_matches(&[1, 2, 3, 1, 2, 3], &Step::HalfOfLen), 12);
        assert_eq!(sum_matches(&[1, 2, 1, 3, 1, 4, 1, 5], &Step::HalfOfLen), 4);
    }


}
