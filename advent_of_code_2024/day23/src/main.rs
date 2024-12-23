use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fs,
};

fn parse(input: &str) -> HashMap<(u8, u8), HashSet<(u8, u8)>> {
    let mut h: HashMap<(u8, u8), HashSet<(u8, u8)>> = HashMap::new();
    for (a0, a1, _, b0, b1, _) in input.bytes().tuples() {
        h.entry((a0, a1)).or_default().insert((b0, b1));
        h.entry((b0, b1)).or_default().insert((a0, a1));
    }
    h
}

fn part1(lan: &HashMap<(u8, u8), HashSet<(u8, u8)>>) -> usize {
    let mut trios = HashSet::new();
    for (a, aa) in lan {
        if a.0 == b't' {
            for b in lan.get(a).expect("a") {
                let bb = lan.get(b).expect("b");
                for c in bb.intersection(aa) {
                    if aa.contains(c) {
                        let mut z = [a, b, c];
                        z.sort_unstable();
                        trios.insert(z);
                    }
                }
            }
        }
    }
    trios.len()
}

fn part2(lan: &HashMap<(u8, u8), HashSet<(u8, u8)>>) -> usize {
    todo!()
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Can't read file");
    let lan = parse(&input);
    println!("{}", part1(&lan));
}
