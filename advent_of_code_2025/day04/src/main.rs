use std::{collections::BTreeMap, fs};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Tile {
    Open,
    Paper,
}

impl From<char> for Tile {
    fn from(c: char) -> Self {
        match c {
            '.' => Self::Open,
            '@' => Self::Paper,
            _ => panic!("Bad char: {c}"),
        }
    }
}

type Grid = BTreeMap<(usize, usize), Tile>;

fn parse(s: &str) -> Grid {
    s.trim()
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.trim()
                .chars()
                .enumerate()
                .map(move |(j, c)| ((i, j), Tile::from(c)))
        })
        .collect()
}

fn part_1(g: &Grid) -> usize {
    g.iter()
        .filter(|&(&(i, j), &t)| {
            t == Tile::Paper && {
                let mut paper = 0;
                for x in i.saturating_sub(1)..=i + 1 {
                    for y in j.saturating_sub(1)..=j + 1 {
                        if (x, y) != (i, j) && g.get(&(x, y)) == Some(&Tile::Paper) {
                            paper += 1;
                        }
                    }
                }
                paper < 4
            }
        })
        .count()
}

fn main() {
    let input = parse(&fs::read_to_string("input.txt").expect("file"));
    println!("{}", part_1(&input));
}
