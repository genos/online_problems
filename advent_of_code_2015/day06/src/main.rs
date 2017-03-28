extern crate regex;
#[macro_use]
extern crate lazy_static;
use regex::Regex;
use std::error::Error;
use std::fs::File;
use std::io::BufReader;
use std::io::Lines;
use std::io::prelude::*;
use std::collections::HashMap;
use std::path::Path;

type Grid = HashMap<(usize, usize), bool>;

fn turn_on(g: &mut Grid, i: usize, j: usize) {
    *g.entry((i, j)).or_insert(true) = true;
}

fn turn_off(g: &mut Grid, i: usize, j: usize) {
    *g.entry((i, j)).or_insert(false) = false;
}

fn toggle(g: &mut Grid, i: usize, j: usize) {
    let e = g.entry((i, j)).or_insert(false);
    *e = !*e;
}

fn parse_pairs(s: &str) -> (usize, usize, usize, usize) {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            "([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"
            ).unwrap();
    }
    match RE.captures(s) {
        None => panic!("Unable to match against {}", s),
        Some(c) => {
            let xs = (0..5)
                .filter_map(|i| c.get(i))
                .filter_map(|x| x.as_str().parse().ok())
                .collect::<Vec<usize>>();
            if xs.len() != 4 {
                panic!("Wanted 4 values, but got {:?}", xs);
            }
            (xs[0], xs[1], xs[2], xs[3])
        }
    }
}

fn parse_and_run(g: &mut Grid, s: &str) {
    let f = if s.starts_with("turn on") {
        turn_on
    } else if s.starts_with("turn off") {
        turn_off
    } else {
        toggle
    };
    let (i0, j0, i1, j1) = parse_pairs(s);
    for i in i0..i1 + 1 {
        for j in j0..j1 + 1 {
            f(g, i, j);
        }
    }
}

#[test]
fn _t() {
    let mut g = Grid::new();
    parse_and_run(&mut g, "turn on 0,0 through 999,999");
    for i in 0..999 {
        for j in 0..999 {
            assert_eq!(Some(&true), g.get(&(i, j)));
        }
    }
    parse_and_run(&mut g, "toggle 0,0 through 999,0");
    for i in 0..999 {
        assert_eq!(Some(&false), g.get(&(i, 0)));
        for j in 1..999 {
            assert_eq!(Some(&true), g.get(&(i, j)));
        }
    }
    parse_and_run(&mut g, "turn off 499,499 through 500,500");
    for i in 0..999 {
        assert_eq!(Some(&false), g.get(&(i, 0)));
        for j in 1..999 {
            if (i == 499 || i == 500) && (j == 499 || j == 500) {
                assert_eq!(Some(&false), g.get(&(i, j)));
            } else {
                assert_eq!(Some(&true), g.get(&(i, j)));
            }
        }
    }
}

fn input() -> Lines<BufReader<File>> {
    let p = Path::new("data/input.txt");
    match File::open(&p) {
        Err(e) => panic!("Couldn't open {}: {}", p.display(), e.description()),
        Ok(f) => BufReader::new(f).lines(),
    }
}

fn answer1(lines: &[String]) -> usize {
    let mut g = Grid::new();
    for line in lines.iter() {
        parse_and_run(&mut g, line.as_str());
    }
    g.values().filter(|i| **i).count()
}

type Grid2 = HashMap<(usize, usize), usize>;

fn on(g: &mut Grid2, i: usize, j: usize) {
    let e = g.entry((i, j)).or_insert(0);
    *e = e.saturating_add(1);
}

fn off(g: &mut Grid2, i: usize, j: usize) {
    let e = g.entry((i, j)).or_insert(0);
    *e = e.saturating_sub(1);
}

fn tog(g: &mut Grid2, i: usize, j: usize) {
    let e = g.entry((i, j)).or_insert(0);
    *e = e.saturating_add(2);
}

fn pnr(g: &mut Grid2, s: &str) {
    let f = if s.starts_with("turn on") {
        on
    } else if s.starts_with("turn off") {
        off
    } else {
        tog
    };
    let (i0, j0, i1, j1) = parse_pairs(s);
    for i in i0..i1 + 1 {
        for j in j0..j1 + 1 {
            f(g, i, j);
        }
    }
}

fn brightness(g: &Grid2) -> usize {
    g.values().sum()
}

#[test]
fn _t2() {
    let mut g = Grid2::new();
    pnr(&mut g, "turn on 0,0 through 0,0");
    assert_eq!(1, brightness(&g));
    pnr(&mut g, "toggle 0,0 through 999,999");
    assert_eq!(1 + 2000000, brightness(&g));
}

fn answer2(lines: &[String]) -> usize {
    let mut g = Grid2::new();
    for line in lines.iter() {
        pnr(&mut g, line.as_str());
    }
    brightness(&g)
}

fn main() {
    let i: Vec<String> = input().filter_map(|l| l.ok()).collect();
    println!("{}", answer1(&i));
    println!("{}", answer2(&i));
}
