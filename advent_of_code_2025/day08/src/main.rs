use itertools::Itertools;
use ordered_float::OrderedFloat;
use std::collections::{BTreeMap, BTreeSet};

type JunctionBox = (u64, u64, u64);

#[allow(clippy::cast_precision_loss)]
fn sqr_euc(a: &JunctionBox, b: &JunctionBox) -> OrderedFloat<f32> {
    let dx = (a.0 as f32) - (b.0 as f32);
    let dy = (a.1 as f32) - (b.1 as f32);
    let dz = (a.2 as f32) - (b.2 as f32);
    OrderedFloat::from(dx * dx + dy * dy + dz * dz)
}

fn parse(s: &str) -> Vec<JunctionBox> {
    s.trim()
        .lines()
        .filter_map(|line| {
            line.splitn(3, ',').collect_tuple().map(|(x, y, z)| {
                (
                    x.parse().expect("x"),
                    y.parse().expect("y"),
                    z.parse().expect("z"),
                )
            })
        })
        .collect()
}

// Union-Find of junction boxes
struct Circuits {
    elts: BTreeMap<JunctionBox, usize>,
    parent: Vec<usize>,
    size: Vec<usize>,
}

impl From<&[JunctionBox]> for Circuits {
    // make_set
    fn from(boxes: &[JunctionBox]) -> Self {
        let size = vec![1; boxes.len()];
        let (parent, elts) = boxes.iter().enumerate().map(|(i, x)| (i, (*x, i))).unzip();
        Self { elts, parent, size }
    }
}

impl Circuits {
    fn find(&mut self, x: &JunctionBox) -> usize {
        let mut i = *self
            .elts
            .get(x)
            .unwrap_or_else(|| panic!("Not inserted: {x:?}"));
        // path halving
        let mut p = self.parent[i];
        while p != i {
            let g = self.parent[p];
            self.parent[i] = g;
            i = p;
            p = g;
        }
        i
    }
    // union by size
    fn union(&mut self, x: &JunctionBox, y: &JunctionBox) {
        let (mut a, mut b) = (self.find(x), self.find(y));
        if a != b {
            if self.size[a] < self.size[b] {
                std::mem::swap(&mut a, &mut b);
            }
            self.parent[b] = a;
            self.size[a] += self.size[b];
        }
    }
}

fn part_1(boxes: &[JunctionBox], n: usize) -> usize {
    let mut circuits = Circuits::from(boxes);
    boxes
        .iter()
        .tuple_combinations()
        .sorted_unstable_by_key(|(a, b)| sqr_euc(a, b))
        .take(n)
        .for_each(|(a, b)| circuits.union(a, b));
    circuits
        .parent
        .iter()
        .unique()
        .map(|&i| circuits.size[i])
        .sorted()
        .rev()
        .take(3)
        .product()
}

fn part_2(boxes: &[JunctionBox]) -> u64 {
    let mut circuits = Circuits::from(boxes);
    let mut ps = BTreeSet::new();
    for (a, b) in boxes
        .iter()
        .tuple_combinations()
        .sorted_unstable_by_key(|(a, b)| sqr_euc(a, b))
    {
        circuits.union(a, b);
        ps.clear();
        for x in boxes {
            ps.insert(circuits.find(x));
        }
        if ps.len() == 1 {
            return a.0 * b.0;
        }
    }
    unreachable!("Unconnected");
}

fn main() {
    let input = std::fs::read_to_string("input.txt").expect("input");
    let boxes = parse(&input);
    println!("{}", part_1(&boxes, 1_000));
    println!("{}", part_2(&boxes));
}
