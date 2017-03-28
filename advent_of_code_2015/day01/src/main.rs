use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn input() -> String {
    let p = Path::new("data/input.txt");
    let mut s = String::new();
    if let Err(e) = File::open(&p).map(|mut f| f.read_to_string(&mut s)) {
        panic!("Couldn't open {}: {}", p.display(), e.description())
    }
    s
}

fn to_i(c: char) -> i64 {
    match c {
        '(' => 1,
        ')' => -1,
        _ => 0,
    }
}

fn consume(i: &str) -> i64 {
    i.chars().map(to_i).sum()
}

fn first_1(i: &str) -> u64 {
    i.chars()
        .map(to_i)
        .fold((0, 0), |(ctr, acc), x| {
            if acc == -1 {
                (ctr, acc)
            } else {
                (ctr + 1, acc + x)
            }
        })
        .0
}

fn main() {
    let i = input();
    println!("{}", consume(&i));
    println!("{}", first_1(&i));
}

#[test]
fn _c0() {
    assert_eq!(0, consume(&"(())".to_string()));
    assert_eq!(0, consume(&"()()".to_string()));
}

#[test]
fn _c3() {
    assert_eq!(3, consume(&"(((".to_string()));
    assert_eq!(3, consume(&"(()(()(".to_string()));
    assert_eq!(3, consume(&"))(((((".to_string()));
}

#[test]
fn _c_1() {
    assert_eq!(-1, consume(&"())".to_string()));
    assert_eq!(-1, consume(&"))(".to_string()));
}

#[test]
fn _c_3() {
    assert_eq!(-3, consume(&")))".to_string()));
    assert_eq!(-3, consume(&")())())".to_string()));
}

#[test]
fn _f1() {
    assert_eq!(1, first_1(&")".to_string()));
}

#[test]
fn _f5() {
    assert_eq!(5, first_1(&"()())".to_string()));
}
