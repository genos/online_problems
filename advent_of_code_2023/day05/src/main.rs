use eyre::{Result, WrapErr};
use itertools::Itertools;
use rayon::prelude::*;
use std::fs;

struct Row {
    destination: u64,
    source: u64,
    length: u64,
}

impl Row {
    fn lookup(&self, i: u64) -> Option<u64> {
        if (self.source <= i) && (i <= self.source + self.length) {
            Some(self.destination + i - self.source)
        } else {
            None
        }
    }
}

struct Almanac {
    seeds: Vec<u64>,
    maps: Vec<Vec<Row>>,
}

impl Almanac {
    fn lookup(&self, seed: u64) -> u64 {
        let mut i = seed;
        self.maps.iter().for_each(|map| {
            i = map.iter().find_map(|row| row.lookup(i)).unwrap_or(i);
        });
        i
    }
}

peg::parser! {
    grammar parser() for str {
        pub(super) rule almanac() -> Almanac = "seeds: " seeds:(int() ** " ") "\n\n" maps:(map() ** "\n\n") { Almanac { seeds, maps } }
        rule map() -> Vec<Row> = ['a'..='z']+ "-" ['a'..='z']+ "-" ['a'..='z']+ " map:\n" rows:(row() ** "\n") { rows }
        rule row() -> Row = destination:int() " " source:int() " " length:int() { Row { destination, source, length} }
        rule int() -> u64 = i:$(['0'..='9']+) {? i.parse::<u64>().or(Err("int")) }
    }
}

fn part1(almanac: &Almanac) -> Option<u64> {
    almanac.seeds.iter().map(|&s| almanac.lookup(s)).min()
}

fn part2(almanac: &Almanac) -> Option<u64> {
    almanac
        .seeds
        .iter()
        .tuples()
        .collect::<Vec<(_, _)>>()
        .par_iter()
        .filter_map(|(&lo, &hi)| (lo..lo + hi).map(|s| almanac.lookup(s)).min())
        .min()
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt").wrap_err("Unable to read input file.")?;
    let almanac = parser::almanac(input.trim()).wrap_err("Bad parse.")?;
    println!("{:?}", part1(&almanac));
    println!("{:?}", part2(&almanac));
    Ok(())
}
