use itertools::Itertools;
use ordered_float::OrderedFloat;
use std::collections::{BTreeMap, BTreeSet};

type JunctionBox = [u64; 3];

#[allow(clippy::cast_precision_loss)]
fn l2sq(a: &JunctionBox, b: &JunctionBox) -> OrderedFloat<f32> {
    OrderedFloat::from(
        a.iter()
            .zip(b)
            .map(|(&a_, &b_)| (a_ as f32 - b_ as f32).powi(2))
            .sum::<f32>(),
    )
}

fn parse(s: &str) -> Vec<JunctionBox> {
    s.trim()
        .lines()
        .filter_map(|line| {
            line.splitn(3, ',')
                .map(|x| x.parse().expect("parse"))
                .collect_array()
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
    fn from(boxes: &[JunctionBox]) -> Self {
        // map(make_set, boxes), in Wikipedia's parlance, over the (unique!) input boxes
        let (parent, elts) = boxes.iter().enumerate().map(|(i, &x)| (i, (x, i))).unzip();
        let size = vec![1; boxes.len()];
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
            self.parent[i] = self.parent[p];
            (i, p) = (p, self.parent[p]);
        }
        i
    }
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

fn part_1(boxes: &[JunctionBox]) -> usize {
    let mut circuits = Circuits::from(boxes);
    boxes
        .iter()
        .tuple_combinations()
        .sorted_unstable_by_key(|(a, b)| l2sq(a, b))
        .take(1_000)
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
    let (mut circuits, mut ps) = (Circuits::from(boxes), BTreeSet::new());
    for (a, b) in boxes
        .iter()
        .tuple_combinations()
        .sorted_unstable_by_key(|(a, b)| l2sq(a, b))
    {
        ps.clear();
        circuits.union(a, b);
        for x in boxes {
            ps.insert(circuits.find(x));
        }
        if ps.len() == 1 {
            return a[0] * b[0];
        }
    }
    unreachable!("Unconnected");
}

fn main() {
    let input = parse(
        &std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/input.txt")).expect("file"),
    );
    println!("{}", part_1(&input));
    println!("{}", part_2(&input));
}
