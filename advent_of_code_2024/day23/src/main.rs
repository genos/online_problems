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

fn main() {
    let input = fs::read_to_string("input.txt").expect("Can't read file");
    let m = parse(&input);
    println!("{m:?}");
}
