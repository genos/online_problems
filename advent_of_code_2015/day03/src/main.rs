use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::collections::HashMap;
use std::path::Path;

type Loc = (i64, i64);
type Count = usize;
type Board = HashMap<Loc, Count>;

static ORIGIN: Loc = (0, 0);

#[derive(Debug, PartialEq, Eq)]
enum Direction {
    North,
    South,
    East,
    West,
}


fn to_dir(c: char) -> Direction {
    match c {
        '^' => Direction::North,
        'v' => Direction::South,
        '>' => Direction::East,
        '<' => Direction::West,
        _ => panic!("Bad char for direction: {}", c),
    }
}

fn new_board() -> Board {
    let mut b = Board::new();
    b.insert(ORIGIN, 1);
    b
}

fn place_and_step(l: Loc, d: &Direction, b: &mut Board) -> Loc {
    let l2 = match *d {
        Direction::North => (l.0, l.1 + 1),
        Direction::South => (l.0, l.1 - 1),
        Direction::East => (l.0 + 1, l.1),
        Direction::West => (l.0 - 1, l.1),
    };
    *b.entry(l2).or_insert(0) += 1;
    l2
}

fn input() -> String {
    let p = Path::new("data/input.txt");
    let mut s = String::new();
    if let Err(e) = File::open(&p).map(|mut f| f.read_to_string(&mut s)) {
        panic!("Couldn't open {}: {}", p.display(), e.description())
    }
    s
}

#[test]
fn _t0() {
    let mut b = new_board();
    let l = place_and_step(ORIGIN, &to_dir('>'), &mut b);
    assert_eq!(l, (1, 0));
    assert_eq!(2, b.len());
    assert_eq!(Some(&1), b.get(&ORIGIN));
    assert_eq!(Some(&1), b.get(&l));
}

#[test]
fn _t1() {
    let mut b = new_board();
    let mut l = ORIGIN;
    for d in "^>v<".chars().map(to_dir) {
        l = place_and_step(l, &d, &mut b);
    }
    assert_eq!(l, ORIGIN);
    assert_eq!(4, b.len());
    assert_eq!(Some(&2), b.get(&ORIGIN));
    assert_eq!(Some(&1), b.get(&(0, 1)));
    assert_eq!(Some(&1), b.get(&(1, 1)));
    assert_eq!(Some(&1), b.get(&(1, 0)));
}

#[test]
fn _t2() {
    let mut b = new_board();
    let mut l = ORIGIN;
    for d in "^v^v^v^v^v".chars().map(to_dir) {
        l = place_and_step(l, &d, &mut b);
    }
    assert_eq!(l, ORIGIN);
    assert_eq!(2, b.len());
    assert_eq!(Some(&6), b.get(&ORIGIN));
    assert_eq!(Some(&5), b.get(&(0, 1)));
}

fn answer1(dirs: &[Direction]) -> Count {
    let mut b = new_board();
    let mut l = ORIGIN;
    for d in dirs {
        l = place_and_step(l, d, &mut b);
    }
    b.len()
}

fn run_both(dirs: &[Direction]) -> (Board, Loc, Loc) {
    let mut b = new_board();
    let mut santa = ORIGIN;
    let mut robo = ORIGIN;
    let mut f = true;
    for d in dirs {
        if f {
            santa = place_and_step(santa, d, &mut b);
            f = false;
        } else {
            robo = place_and_step(robo, d, &mut b);
            f = true;
        }
    }
    (b, santa, robo)
}

#[test]
fn _t3() {
    let b = run_both(&"^v".chars().map(to_dir).collect()).0;
    assert_eq!(3, b.len());
}

#[test]
fn _t4() {
    let (b, s, r) = run_both(&"^>v<".chars().map(to_dir).collect());
    assert_eq!(3, b.len());
    assert_eq!(ORIGIN, s);
    assert_eq!(ORIGIN, r);
}

#[test]
fn _t5() {
    let (b, s, r) = run_both(&"^v^v^v^v^v".chars().map(to_dir).collect());
    assert_eq!(11, b.len());
    assert_eq!((0, 5), s);
    assert_eq!((0, -5), r);
}

fn answer2(dirs: &[Direction]) -> Count {
    run_both(dirs).0.len()
}

fn main() {
    let d: Vec<_> = input().chars().map(to_dir).collect();
    println!("{}", answer1(&d));
    println!("{}", answer2(&d));
}
