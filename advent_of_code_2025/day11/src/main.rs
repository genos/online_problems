/* Petgraph version that is too slow for part 2
use petgraph::{
    acyclic::Acyclic,
    algo::{all_simple_paths, has_path_connecting},
    prelude::*,
};
use std::collections::{HashMap, HashSet, hash_map::RandomState};

type Servers = Acyclic<DiGraph<String, ()>>;

fn parse(s: &str) -> (Servers, HashMap<String, NodeIndex>) {
    let mut servers = DiGraph::new();
    let nodes = s
        .replace(": ", " ")
        .replace('\n', " ")
        .trim()
        .split_ascii_whitespace()
        .collect::<HashSet<_>>()
        .iter()
        .map(|&x| {
            let n = x.to_string();
            (n.clone(), servers.add_node(n))
        })
        .collect::<HashMap<_, _>>();
    for line in s.trim().lines() {
        let (head, rest) = line.split_once(": ").expect("head");
        let u = nodes[head];
        for v in rest.split_ascii_whitespace() {
            servers.add_edge(u, nodes[v], ());
        }
    }
    (Acyclic::try_from_graph(servers).expect("acyclic"), nodes)
}

fn part_1(servers: &Servers, nodes: &HashMap<String, NodeIndex>) -> usize {
    all_simple_paths::<Vec<_>, _, RandomState>(servers, nodes["you"], nodes["out"], 0, None).count()
}

fn part_2(servers: &Servers, nodes: &HashMap<String, NodeIndex>) -> usize {
    let (svr, dac, fft, out) = (nodes["svr"], nodes["dac"], nodes["fft"], nodes["out"]);
    if has_path_connecting(servers, dac, fft, None) {
        println!("dac -> fft");
        let x = all_simple_paths::<Vec<_>, _, RandomState>(servers, svr, dac, 0, None).count();
        let y = all_simple_paths::<Vec<_>, _, RandomState>(servers, dac, fft, 0, None).count();
        let z = all_simple_paths::<Vec<_>, _, RandomState>(servers, fft, out, 0, None).count();
        x * y * z
    } else {
        println!("fft -> dac");
        let x = all_simple_paths::<Vec<_>, _, RandomState>(servers, svr, fft, 0, None).count();
        let y = all_simple_paths::<Vec<_>, _, RandomState>(servers, fft, dac, 0, None).count();
        let z = all_simple_paths::<Vec<_>, _, RandomState>(servers, dac, out, 0, None).count();
        x * y * z
    }
}

fn main() {
    let (servers, nodes) = parse(&std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", part_1(&servers, &nodes));
    println!("{}", part_2(&servers, &nodes));
}
*/

// Some help from https://github.com/Fadi88/AoC/blob/master/2025/days/day11/src/lib.rs
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

fn num_paths<'a>(
    u: &'a str,
    v: &'a str,
    servers: &'a HashMap<&'a str, Vec<&'a str>>,
    cache: &mut HashMap<(&'a str, &'a str), u64>,
) -> u64 {
    if u == v {
        1
    } else if let Some(n) = cache.get(&(u, v)) {
        *n
    } else {
        let n = servers
            .get(u)
            .map(|ws| ws.iter().map(|w| num_paths(w, v, servers, cache)).sum())
            .unwrap_or_default();
        cache.insert((u, v), n);
        n
    }
}

fn part_1(servers: &HashMap<&str, Vec<&str>>) -> u64 {
    num_paths("you", "out", servers, &mut HashMap::new())
}

fn part_2(servers: &HashMap<&str, Vec<&str>>) -> u64 {
    let svr_dac = num_paths("svr", "dac", servers, &mut HashMap::new());
    let dac_fft = num_paths("dac", "fft", servers, &mut HashMap::new());
    let fft_out = num_paths("fft", "out", servers, &mut HashMap::new());
    let svr_fft = num_paths("svr", "fft", servers, &mut HashMap::new());
    let fft_dac = num_paths("fft", "dac", servers, &mut HashMap::new());
    let dac_out = num_paths("dac", "out", servers, &mut HashMap::new());
    svr_dac * dac_fft * fft_out + svr_fft * fft_dac * dac_out
}

fn main() {
    let input = std::fs::read_to_string("input.txt").expect("file");
    let servers = parse(&input);
    println!("{}", part_1(&servers));
    println!("{}", part_2(&servers));
}
