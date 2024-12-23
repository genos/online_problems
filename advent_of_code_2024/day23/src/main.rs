use itertools::iproduct;
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

fn part1(network: &HashMap<&str, HashSet<&str>>) -> usize {
    let mut trios = HashSet::new();
    for (a, b, c) in iproduct!(network.keys(), network.keys(), network.keys()) {
        let (aa, bb) = (network.get(a).expect("a"), network.get(b).expect("b"));
        if (aa.contains(b) && aa.contains(c) && bb.contains(c))
            && (a.starts_with('t') || b.starts_with('t') || c.starts_with('t'))
        {
            let mut z = [a, b, c];
            z.sort_unstable();
            trios.insert(z);
        }
    }
    trios.len()
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Can't read file");
    let network = parse(&input);
    println!("{}", part1(&network));
}
