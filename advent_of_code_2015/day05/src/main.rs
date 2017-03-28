use std::error::Error;
use std::fs::File;
use std::io::BufReader;
use std::io::Lines;
use std::io::prelude::*;
use std::collections::HashSet;
use std::path::Path;

fn zip_skip<T>(ts: &[T], i: usize) -> Vec<(&T, &T)> {
    ts.iter().zip(ts.iter().skip(i)).collect()
}

fn three_vowels(s: &str) -> bool {
    let vowels = "aeiou".chars().collect::<HashSet<_>>();
    s.chars().filter(|c| vowels.contains(c)).collect::<Vec<_>>().len() >= 3
}

fn double_letter(s: &str) -> bool {
    if s.len() < 2 {
        false
    } else {
        for (x, y) in zip_skip(s.as_bytes(), 1) {
            if x == y {
                return true;
            }
        }
        false
    }
}

fn bad_combos(s: &str) -> bool {
    let bads = [(b'a', b'b'), (b'c', b'd'), (b'p', b'q'), (b'x', b'y')]
        .iter()
        .cloned()
        .collect::<HashSet<_>>();
    if s.len() < 2 {
        false
    } else {
        for (x, y) in zip_skip(s.as_bytes(), 1) {
            if bads.contains(&(*x, *y)) {
                return true;
            }
        }
        false
    }
}

fn nice(s: &str) -> bool {
    three_vowels(s) && double_letter(s) && !bad_combos(s)
}

#[test]
fn _t0() {
    assert!(nice("ugknbfddgicrmopn"));
    assert!(nice("aaa"));
    assert!(!nice("jchzalrnumimnmhp"));
    assert!(!nice("haegwjzuvuyypxyu"));
    assert!(!nice("dvszwmarrgswjxmb"));
}

fn input() -> Lines<BufReader<File>> {
    let p = Path::new("data/input.txt");
    match File::open(&p) {
        Err(e) => panic!("Couldn't open {}: {}", p.display(), e.description()),
        Ok(f) => BufReader::new(f).lines(),
    }
}

fn answer1(ss: &[String]) -> usize {
    ss.iter().filter(|s| nice(s.as_str())).count()
}

fn two_pair_no_overlap(s: &str) -> bool {
    if s.len() < 4 {
        false
    } else {
        for i in 0..s.len() - 2 {
            let (a, b) = s.split_at(i + 2);
            let c = a.split_at(i).1;
            if b.contains(c) {
                return true;
            }
        }
        false
    }
}

fn aba(s: &str) -> bool {
    if s.len() < 3 {
        false
    } else {
        for (x, y) in zip_skip(s.as_bytes(), 2) {
            if x == y {
                return true;
            }
        }
        false
    }
}

fn nice2(s: &str) -> bool {
    two_pair_no_overlap(s) && aba(s)
}

#[test]
fn _t1() {
    assert!(nice2("qjhvhtzxzqqjkmpb"));
    assert!(nice2("xxyxx"));
    assert!(!nice2("uurcxstgmygtbstg"));
    assert!(!nice2("ieodomkazucvgmuy"));
}

fn answer2(ss: &[String]) -> usize {
    ss.iter().filter(|s| nice2(s.as_str())).count()
}

fn main() {
    let i: Vec<String> = input().filter_map(|l| l.ok()).collect();
    println!("{}", answer1(&i));
    println!("{}", answer2(&i))
}
