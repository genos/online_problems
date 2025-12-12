// Some help from https://github.com/Fadi88/AoC/blob/master/2025/days/day11/src/lib.rs
// (Not much help; I had the entire thing right, but used petgraph poorly.)
// petgraph does _too_ much! We need simplicity.

use std::collections::HashMap;

fn parse(s: &str) -> HashMap<&str, Vec<&str>> {
    s.trim()
        .lines()
        .map(|line| {
            let (u, vs) = line.split_once(": ").expect("format");
            (u, vs.split_ascii_whitespace().collect())
        })
        .collect()
}

fn paths(source: &str, target: &str, servers: &HashMap<&str, Vec<&str>>) -> u64 {
    fn go<'a>(
        u: &'a str,
        v: &'a str,
        d: &HashMap<&'a str, Vec<&'a str>>,
        cache: &mut HashMap<&'a str, u64>,
    ) -> u64 {
        if u == v {
            1
        } else if let Some(n) = cache.get(u) {
            *n
        } else {
            let n = d
                .get(u)
                .map(|ws| ws.iter().map(|w| go(w, v, d, cache)).sum())
                .unwrap_or_default();
            cache.insert(u, n);
            n
        }
    }
    go(source, target, servers, &mut HashMap::with_capacity(servers.len()))
}

fn part_1(servers: &HashMap<&str, Vec<&str>>) -> u64 {
    paths("you", "out", servers)
}

fn part_2(servers: &HashMap<&str, Vec<&str>>) -> u64 {
    [("dac", "fft"), ("fft", "dac")]
        .iter()
        .map(|(u, v)| paths("svr", u, servers) * paths(u, v, servers) * paths(v, "out", servers))
        .sum()
}

fn main() {
    let input = parse(include_str!("../input.txt"));
    println!("{}", part_1(&input));
    println!("{}", part_2(&input));
}
