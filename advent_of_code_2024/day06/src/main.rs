use eyre::{eyre, Result, WrapErr};
use itertools::Itertools;
use rayon::prelude::*;
use std::{collections::BTreeSet, fs};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct Coord(u8, u8);

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
            Dir::D => Coord(i.wrapping_sub(1), j),
            Dir::L => Coord(i, j.wrapping_sub(1)),
            Dir::R => Coord(i, j + 1),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct Guard {
    pos: Coord,
    dir: Dir,
}

trait Trail: Ord + Sized {
    fn f(g: Guard) -> Self;
    fn stop_early(&self, visited: &BTreeSet<Self>) -> bool;
}
impl Trail for Coord {
    fn f(g: Guard) -> Self {
        g.pos
    }
    fn stop_early(&self, _visited: &BTreeSet<Self>) -> bool {
        false
    }
}
impl Trail for (Coord, Dir) {
    fn f(g: Guard) -> Self {
        (g.pos, g.dir)
    }
    fn stop_early(&self, visited: &BTreeSet<Self>) -> bool {
        visited.contains(self)
    }
}

enum Walk {
    EarlyStop(usize),
    OutOfBounds(usize),
    Bail,
}

fn ix(n: u8, i: u8, j: u8) -> usize {
    n as usize * j as usize + i as usize
}

#[derive(Clone)]
struct Map(u8, Vec<bool>);

impl Map {
    fn contains(&self, p: Coord) -> bool {
        p.0 < self.0 && p.1 < self.0
    }
    fn get(&self, p: Coord) -> bool {
        self.1[ix(self.0, p.0, p.1)]
    }
    fn set(&mut self, p: Coord, c: bool) {
        self.1[ix(self.0, p.0, p.1)] = c;
    }
    fn parse(n: usize, input: &str) -> Result<(Self, Guard)> {
        let mut data = vec![false; n * n];
        let n: u8 = n.try_into().wrap_err("n too big")?;
        let mut pos = Coord(255, 255);
        for (i, line) in input.lines().rev().enumerate() {
            for (j, c) in line.chars().enumerate() {
                let i: u8 = i.try_into().wrap_err("i too big")?;
                let j: u8 = j.try_into().wrap_err("i too big")?;
                data[ix(n, i, j)] = c == '#';
                if c == '^' {
                    pos = Coord(i, j);
                }
            }
        }
        if pos == Coord(255, 255) {
            Err(eyre!("No ^ found."))
        } else {
            Ok((Self(n, data), Guard { pos, dir: Dir::U }))
        }
    }
    fn go<T: Trail>(&self, mut g: Guard) -> Walk {
        let mut visited = BTreeSet::from([T::f(g)]);
        let mut n = 0;
        while n < self.1.len() {
            let ahead = g.dir.step(g.pos);
            if self.contains(ahead) {
                if self.get(ahead) {
                    g.dir = g.dir.turn_right();
                } else {
                    g.pos = ahead;
                }
                let trail = T::f(g);
                if trail.stop_early(&visited) {
                    return Walk::EarlyStop(visited.len());
                }
                visited.insert(trail);
            } else {
                return Walk::OutOfBounds(visited.len());
            }
            n += 1;
        }
        Walk::Bail
    }
}

fn part1(m: &Map, g: Guard) -> Result<usize> {
    match m.go::<Coord>(g) {
        Walk::EarlyStop(n) | Walk::OutOfBounds(n) => Ok(n),
        Walk::Bail => Err(eyre!("Bigger than map's size")),
    }
}

fn part2(m: &Map, g: Guard) -> usize {
    (0..m.0)
        .cartesian_product(0..m.0)
        .filter(|&ij| ij != (g.pos.0, g.pos.1))
        .par_bridge()
        .filter(|&(i, j)| {
            let mut m_ = m.clone();
            m_.set(Coord(i, j), true);
            matches!(m_.go::<(Coord, Dir)>(g), Walk::EarlyStop(_))
        })
        .count()
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt").wrap_err("Unable to read input file.")?;
    let n = input.chars().filter(|c| *c == '\n').count();
    let (m, g) = Map::parse(1 + n, &input)?;
    println!("{}", part1(&m, g)?);
    println!("{}", part2(&m, g));
    Ok(())
}
