use petgraph::{algo::all_simple_paths, prelude::*};
use std::collections::{HashMap, HashSet};

fn parse(s: &str) -> (DiGraph<String, ()>, HashMap<String, NodeIndex>) {
    let mut servers = DiGraph::new();
    let nodes = s
        .replace(": ", " ")
        .replace("\n", " ")
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
    (servers, nodes)
}

fn part_1(servers: &DiGraph<String, ()>, nodes: &HashMap<String, NodeIndex>) -> usize {

    0
}

fn main() {
    let (servers, nodes) = parse(&std::fs::read_to_string("test.txt").expect("file"));
    println!("{servers:?}");
    println!("{nodes:?}");
    println!("{}", part_1(&servers, &nodes));
}
