use std::{collections::BTreeMap, fs};

#[derive(PartialEq, Clone)]
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

fn removable(i: usize, j: usize, g: &Grid) -> bool {
    let mut paper = 0u8;
    for x in i.saturating_sub(1)..=i + 1 {
        for y in j.saturating_sub(1)..=j + 1 {
            if (x, y) != (i, j) && g.get(&(x, y)) == Some(&Tile::Paper) {
                paper += 1;
            }
        }
    }
    paper < 4
}

fn part_1(g: &Grid) -> usize {
    g.iter()
        .filter(|&(&(i, j), t)| t == &Tile::Paper && removable(i, j, g))
        .count()
}

fn part_2(g: &Grid) -> usize {
    let (mut lookup, mut mutate, mut cleared) = (g.clone(), g.clone(), 0);
    loop {
        let mut count = 0;
        for (&(i, j), t) in &mut mutate {
            if t == &Tile::Paper && removable(i, j, &lookup) {
                *t = Tile::Open;
                count += 1;
            }
        }
        cleared += count;
        lookup = mutate.clone();
        if count == 0 {
            break;
        }
    }
    cleared
}

fn main() {
    let input = parse(&fs::read_to_string("input.txt").expect("file"));
    println!("{}", part_1(&input));
    println!("{}", part_2(&input));
}
