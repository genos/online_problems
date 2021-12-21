use std::fs;
use std::path::Path;
use anyhow::Result;
#[macro_use]
extern crate scan_fmt;

mod part1;
mod part2;

fn parse<F, G>(f: F, s: &str) -> Result<G>
where
    F: Fn(u64, u64) -> G,
{
    let (pos1, pos2) = scan_fmt!(
        s,
        "Player 1 starting position: {d}\nPlayer 2 starting position: {d}",
        u64,
        u64
    )?;
    Ok(f(pos1, pos2))
}

fn main() {
    let input = fs::read_to_string(&Path::new("input.txt")).expect("Can't read file");
    let mut game1 = parse(part1::Game::new, &input).expect("Can't parse game");
    println!("Part 1: {}", game1.play());
}
