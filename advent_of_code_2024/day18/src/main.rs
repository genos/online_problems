use eyre::{OptionExt, Result};
use pathfinding::prelude::astar;
use std::{collections::BTreeSet, fs};

const LO: u16 = 0;
const HI: u16 = 70;
type Coord = (u16, u16);

struct MemorySpace(BTreeSet<Coord>);

peg::parser! {
    grammar parser() for str {
        pub rule coords(n: usize) -> Vec<Coord> = cs:(coord() ** "\n") { cs.into_iter().take(n).collect() }
        rule coord() -> Coord = u:num() "," v:num() { (u, v) }
        rule num() -> u16 = n:$(['0'..='9']['0'..='9']*) {? n.parse().or(Err("num")) }
    }
}

impl MemorySpace {
    fn successors(&self, c: Coord) -> Vec<(Coord, u16)> {
        let (x, y) = c;
        [
            (x.saturating_sub(1), y),
            (x.saturating_add(1).min(HI), y),
            (x, y.saturating_sub(1)),
            (x, y.saturating_add(1).min(HI)),
        ]
        .into_iter()
        .filter_map(|p| (!self.0.contains(&p)).then_some((p, 1)))
        .collect()
    }
    fn search(&self) -> Option<(Vec<Coord>, u16)> {
        astar(
            &(LO, LO),
            |&c| self.successors(c),
            |&c| c.0.abs_diff(HI) + c.1.abs_diff(HI),
            |&c| c == (HI, HI),
        )
    }
}

fn part1(input: &str) -> Option<u16> {
    MemorySpace(parser::coords(input, 1024).ok()?.into_iter().collect())
        .search()
        .map(|(_path, cost)| cost)
}

fn part2(input: &str) -> Option<String> {
    let n = input.chars().filter(|c| *c == '\n').count();
    (0..n).find_map(|i| {
        let cs = parser::coords(input, i).ok()?;
        if MemorySpace(cs.iter().copied().collect()).search().is_none() {
            let &(x, y) = cs.last()?;
            Some(format!("{x},{y}"))
        } else {
            None
        }
    })
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    println!("{}", part1(&input).ok_or_eyre("part 1 failed")?);
    println!("{}", part2(&input).ok_or_eyre("part 2 failed")?);
    Ok(())
}
