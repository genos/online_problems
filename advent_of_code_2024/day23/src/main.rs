use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fs,
};

fn parse(input: &str) -> HashMap<&str, HashSet<&str>> {
    let mut h: HashMap<&str, HashSet<&str>> = HashMap::new();
    for line in input.lines() {
        let u = &line[0..2];
        let v = &line[3..5];
        h.entry(u).or_default().insert(v);
        h.entry(v).or_default().insert(u);
    }
    h
}

fn part1(lan: &HashMap<&str, HashSet<&str>>) -> usize {
    let mut trios = HashSet::new();
    for (a, aa) in lan {
        if a.starts_with('t') {
            for b in lan.get(a).expect("a") {
                let bb = lan.get(b).expect("b");
                for c in bb.intersection(aa) {
                    if aa.contains(c) {
                        let mut z = [a, b, c];
                        z.sort();
                        trios.insert(z);
                    }
                }
            }
        }
    }
    trios.len()
}

fn part2(lan: &HashMap<&str, HashSet<&str>>) -> String {
    let mut cliques = lan
        .keys()
        .map(|&k| HashSet::from_iter([k]))
        .collect::<Vec<HashSet<&str>>>();
    for c in &mut cliques {
        for a in lan.keys() {
            if c.iter().all(|b| lan[a].contains(b)) {
                c.insert(a);
            }
        }
    }
    cliques
        .iter()
        .max_by_key(|c| c.len())
        .expect("nonempty")
        .iter()
        .sorted()
        .join(",")
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Can't read file");
    let lan = parse(&input);
    println!("{}", part1(&lan));
    println!("{}", part2(&lan));
}
