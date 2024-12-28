use eyre::{OptionExt, Result};
use pathfinding::prelude::astar;
use rayon::prelude::*;
use std::{collections::BTreeSet, fs};

const LO: u8 = 0;
const HI: u8 = 70;
type Coord = (u8, u8);

fn parse(input: &str) -> Result<Vec<Coord>> {
    input
        .lines()
        .map(|l| {
            l.split_once(',')
                .ok_or_eyre("split")
                .and_then(|(x, y)| Ok((x.parse()?, y.parse()?)))
        })
        .collect()
}

fn search(coords: &BTreeSet<&Coord>) -> Option<(Vec<Coord>, u16)> {
    astar(
        &(LO, LO),
        |&(x, y): &Coord| {
            [
                (x.saturating_sub(1), y),
                (x.saturating_add(1).min(HI), y),
                (x, y.saturating_sub(1)),
                (x, y.saturating_add(1).min(HI)),
            ]
            .into_iter()
            .filter_map(|p| (!coords.contains(&p)).then_some((p, 1)))
        },
        |&(x, y)| u16::from(x.abs_diff(HI)) + u16::from(y.abs_diff(HI)),
        |&(x, y)| x == HI && y == HI,
    )
}

fn part1(coords: &[Coord]) -> Option<u16> {
    search(&coords.iter().take(1024).collect()).map(|(_path, cost)| cost)
}

fn part2(coords: &[Coord], n: usize) -> Option<String> {
    (0..n)
        .collect::<Vec<_>>()
        .into_par_iter()
        .find_first(|&i| search(&coords.iter().take(i).collect()).is_none())
        .map(|i| {
            let (x, y) = coords[i - 1];
            format!("{x},{y}")
        })
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let coords = parse(&input)?;
    println!("{}", part1(&coords).ok_or_eyre("part 1")?);
    let n = input.lines().count();
    println!("{}", part2(&coords, n).ok_or_eyre("part 2")?);
    Ok(())
}
