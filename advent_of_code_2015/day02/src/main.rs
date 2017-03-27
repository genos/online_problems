use std::error::Error;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use std::path::Path;

macro_rules! min {
    ($x:expr) => ( $x );
    ($x:expr, $($xs:expr),+) => {
        {
            use std::cmp::min;
            min($x, min!( $($xs),+ ))
        }
    };
}

fn input() -> Vec<String> {
    let path = Path::new("data/input.txt");
    let display = path.display();
    let file = match File::open(&path) {
        Err(why) => panic!("Couldn't open {}: {}", display, why.description()),
        Ok(file) => file,
    };
    BufReader::new(file)
        .lines()
        .map(|x| match x {
            Err(why) => panic!("Couldn't open {}: {}", display, why.description()),
            Ok(s) => s,
        })
        .collect()
}

fn to_dims(s: &str) -> (u64, u64, u64) {
    let v: Vec<u64> = s.split('x').filter_map(|x| x.parse::<u64>().ok()).collect();
    match v.len() {
        3 => (v[0], v[1], v[2]),
        _ => panic!("Invalid input: {}", s),
    }
}

fn sf(lwh: &(u64, u64, u64)) -> u64 {
    let s0 = lwh.0 * lwh.1;
    let s1 = lwh.0 * lwh.2;
    let s2 = lwh.1 * lwh.2;
    2 * (s0 + s1 + s2) + min!(s0, s1, s2)
}

#[test]
fn _t0() {
    assert_eq!(58, sf(&to_dims("2x3x4")));
}

#[test]
fn _t1() {
    assert_eq!(43, sf(&to_dims("1x1x10")));
}

fn r(lwh: &(u64, u64, u64)) -> u64 {
    let p0 = 2 * (lwh.0 + lwh.1);
    let p1 = 2 * (lwh.0 + lwh.2);
    let p2 = 2 * (lwh.1 + lwh.2);
    min!(p0, p1, p2) + lwh.0 * lwh.1 * lwh.2
}

#[test]
fn _t2() {
    assert_eq!(34, r(&to_dims("2x3x4")));
}

#[test]
fn _t3() {
    assert_eq!(14, r(&to_dims("1x1x10")));
}

fn main() {
    let d: Vec<(u64, u64, u64)> = input().iter().map(|l| to_dims(l)).collect();
    let (a1, a2) = d.iter().map(|x| (sf(x), r(x))).fold((0, 0), |(a, b), (x, y)| (a + x, b + y));
    println!("{}", a1);
    println!("{}", a2);
}
