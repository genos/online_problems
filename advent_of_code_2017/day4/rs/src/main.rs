use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;

fn is_valid_passphrase(s: &str) -> bool {
    let mut ws = HashSet::new();
    for w in s.split_whitespace() {
        if ws.contains(w) {
            return false;
        }
        ws.insert(w);
    }
    true
}

fn read_input() -> String {
    let mut f = File::open("../input.txt").expect("'input.txt' not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("unable to read 'input.txt'");
    contents
}

fn part1(input: &str) -> usize {
    input.lines().filter(|l| is_valid_passphrase(l)).count()
}

fn is_valid_enhanced(s: &str) -> bool {
    let mut ws = HashSet::new();
    for w in s.split_whitespace() {
        let mut ww = w.chars().collect::<Vec<_>>();
        ww.sort();
        if ws.contains(&ww) {
            return false;
        }
        ws.insert(ww);
    }
    true
}

fn part2(input: &str) -> usize {
    input.lines().filter(|l| is_valid_enhanced(l)).count()
}

fn main() {
    let input = read_input();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_valid_passphrase() {
        assert!(is_valid_passphrase("aa bb cc dd ee"));
        assert!(!is_valid_passphrase("aa bb cc dd aa"));
        assert!(is_valid_passphrase("aa bb cc dd aaa"));
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1("aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa"), 2);
    }

    #[test]
    fn test_is_valid_enhanced() {
        assert!(is_valid_enhanced("abcde fghij"));
        assert!(!is_valid_enhanced("abcde xyz edcba"));
        assert!(is_valid_enhanced("a ab abc abd abe abf abj"));
        assert!(is_valid_enhanced("iiii oiii ooii oooi oooo"));
        assert!(!is_valid_enhanced("oiii ioii iioi iiio"));
    }

    #[test]
    fn test_part2() {
        assert_eq!(
            part2(
                "abcde fghij\nabcde xyz edcba\na ab abc abd abe abf abj\niiii oiii ooii oooi oooo"
            ),
            3
        );
    }
}
