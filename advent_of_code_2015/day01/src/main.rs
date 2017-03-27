use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn input() -> String {
    let path = Path::new("data/input.txt");
    let display = path.display();
    let mut file = match File::open(&path) {
        Err(why) => panic!("Couldn't open {}: {}", display, why.description()),
        Ok(file) => file,
    };
    let mut s = String::new();
    if let Err(why) = file.read_to_string(&mut s) {
        panic!("Couldn't open {}: {}", display, why.description())
    }
    s
}

fn to_i(c: char) -> i64 {
    if c == '(' {
        1
    } else if c == ')' {
        -1
    } else {
        0
    }
}

fn consume(i: &str) -> i64 {
    i.chars().map(to_i).fold(0, |acc, x| acc + x)
}

fn first_1(i: &str) -> u64 {
    let (c, _) = i.chars().map(to_i).fold((0, 0), |(ctr, acc), x| {
        if acc == -1 {
            (ctr, acc)
        } else {
            (ctr + 1, acc + x)
        }
    });
    c
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
