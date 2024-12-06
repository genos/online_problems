use eyre::{eyre, Result, WrapErr};
use itertools::Itertools;
use rayon::prelude::*;
use std::{collections::BTreeSet, fs, marker::PhantomData};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct Coord(u8, u8);

impl Coord {
    fn in_bounds(self, n: u8) -> bool {
        self.0 < n && self.1 < n
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Dir {
    U,
    D,
    L,
    R,
}

impl Dir {
    fn turn_right(self) -> Self {
        match self {
            Self::U => Self::R,
            Self::D => Self::L,
            Self::L => Self::U,
            Self::R => Self::D,
        }
    }
}

impl Dir {
    fn step(self, c: Coord) -> Coord {
        let (i, j) = (c.0, c.1);
        match self {
            Dir::U => Coord(i + 1, j),
            Dir::D => Coord(i - 1, j),
            Dir::L => Coord(i, j - 1),
            Dir::R => Coord(i, j + 1),
        }
    }
}

trait Trail: Ord + Sized {
    fn f(pos: Coord, dir: Dir) -> Self;
    fn stop_early(&self, visited: &BTreeSet<Self>) -> bool;
}
impl Trail for Coord {
    fn f(pos: Coord, _dir: Dir) -> Self {
        pos
    }
    fn stop_early(&self, _visited: &BTreeSet<Self>) -> bool {
        false
    }
}
impl Trail for (Coord, Dir) {
    fn f(pos: Coord, dir: Dir) -> Self {
        (pos, dir)
    }
    fn stop_early(&self, visited: &BTreeSet<Self>) -> bool {
        visited.contains(self)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct Guard<T: Trail> {
    pos: Coord,
    dir: Dir,
    trail: PhantomData<T>,
}

enum Walk {
    EarlyStop(usize),
    OutOfBounds(usize),
}

macro_rules! ix {
    ($n:expr, $i:expr, $j:expr) => {
        $n as usize * $j as usize + $i as usize
    };
}

#[derive(Clone)]
struct Map(u8, Vec<char>);

impl Map {
    fn get(&self, p: Coord) -> char {
        self.1[ix!(self.0, p.0, p.1)]
    }
    fn set(&mut self, p: Coord, c: char) {
        self.1[ix!(self.0, p.0, p.1)] = c;
    }
    fn parse(n: usize, input: &str) -> Result<Self> {
        let mut data = vec!['.'; n * n];
        let n: u8 = n.try_into().wrap_err("n too big")?;
        for (i, line) in input.lines().rev().enumerate() {
            for (j, c) in line.chars().enumerate() {
                let i: u8 = i.try_into().wrap_err("i too big")?;
                let j: u8 = j.try_into().wrap_err("i too big")?;
                data[ix!(n, i, j)] = c;
            }
        }
        Ok(Self(n, data))
    }
    fn find_caret(&self) -> Result<Coord> {
        (0..self.0)
            .cartesian_product(0..self.0)
            .find_map(|(i, j)| (self.1[ix!(self.0, i, j)] == '^').then_some(Coord(i, j)))
            .ok_or(eyre!("No ^ found."))
    }
    fn start<T: Trail>(&mut self) -> Result<Guard<T>> {
        let pos = self.find_caret()?;
        self.set(pos, '.');
        let dir = Dir::U;
        let trail = PhantomData;
        Ok(Guard { pos, dir, trail })
    }
    fn go<T: Trail>(&self, mut g: Guard<T>) -> Walk {
        // Assumes input is well-formed (terminating)
        let mut visited = BTreeSet::new();
        loop {
            let ahead = g.dir.step(g.pos);
            if ahead.in_bounds(self.0) {
                if self.get(ahead) == '#' {
                    g.dir = g.dir.turn_right();
                } else {
                    g.pos = ahead;
                }
                let trail = <T as Trail>::f(g.pos, g.dir);
                if trail.stop_early(&visited) {
                    return Walk::EarlyStop(visited.len());
                }
                visited.insert(trail);
            } else {
                return Walk::OutOfBounds(1 + visited.len());
            }
        }
    }
}

fn part1(mut m: Map) -> Result<usize> {
    let g = m.start::<Coord>()?;
    match m.go(g) {
        Walk::EarlyStop(n) | Walk::OutOfBounds(n) => Ok(n),
    }
}

fn part2(mut m: Map) -> Result<usize> {
    let g = m.start::<(Coord, Dir)>()?;
    let p0 = g.pos;
    let n = (0..m.0)
        .cartesian_product(0..m.0)
        .par_bridge()
        .filter(|(i, j)| {
            let p = Coord(*i, *j);
            if p == p0 {
                false
            } else {
                let mut m_ = m.clone();
                m_.set(p, '#');
                matches!(m_.go(g), Walk::EarlyStop(_))
            }
        })
        .count();
    Ok(n)
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt").wrap_err("Unable to read input file.")?;
    let n = input.chars().filter(|c| *c == '\n').count();
    let map = Map::parse(1 + n, &input)?;
    println!("{}", part1(map.clone())?);
    println!("{}", part2(map)?);
    Ok(())
}
