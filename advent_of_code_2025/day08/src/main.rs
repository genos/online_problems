use itertools::Itertools;
use ordered_float::OrderedFloat;
use std::{
    cmp::{Ordering, Reverse},
    collections::{BTreeMap, BinaryHeap},
};

type JunctionBox = (u32, u32, u32);

#[derive(PartialEq, Eq)]
struct Pair {
    first: JunctionBox,
    second: JunctionBox,
    dist: OrderedFloat<f64>,
}

impl From<(JunctionBox, JunctionBox)> for Pair {
    fn from((first, second): (JunctionBox, JunctionBox)) -> Self {
        let dx = f64::from(first.0) - f64::from(second.0);
        let dy = f64::from(first.1) - f64::from(second.1);
        let dz = f64::from(first.2) - f64::from(second.2);
        let dist = OrderedFloat::from(dx * dx + dy * dy + dz * dz);
        Self {
            first,
            second,
            dist,
        }
    }
}

impl Ord for Pair {
    fn cmp(&self, other: &Self) -> Ordering {
        self.dist
            .cmp(&other.dist)
            .then((self.first, self.second).cmp(&(other.first, other.second)))
    }
}

impl PartialOrd for Pair {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn parse(s: &str) -> Vec<JunctionBox> {
    s.trim()
        .lines()
        .filter_map(|line| {
            line.split_once(',').and_then(|(x, yz)| {
                yz.split_once(',').map(|(y, z)| {
                    (
                        x.parse().expect("x"),
                        y.parse().expect("y"),
                        z.parse().expect("z"),
                    )
                })
            })
        })
        .collect()
}

// Union-Find data structure of junction boxes.
struct Circuits {
    elts_to_ix: BTreeMap<JunctionBox, usize>,
    ix_to_elts: Vec<JunctionBox>,
    parent: Vec<usize>,
    size: Vec<usize>,
}

impl From<&[JunctionBox]> for Circuits {
    fn from(boxes: &[JunctionBox]) -> Self {
        let mut c = Self {
            elts_to_ix: BTreeMap::new(),
            ix_to_elts: Vec::new(),
            parent: Vec::new(),
            size: Vec::new(),
        };
        for j in boxes {
            let _ = c.make_set(j);
        }
        c
    }
}

impl Circuits {
    fn make_set(&mut self, x: &JunctionBox) -> usize {
        self.elts_to_ix.get(x).copied().unwrap_or_else(|| {
            let n = self.parent.len();
            self.elts_to_ix.insert(*x, n);
            self.ix_to_elts.push(*x);
            self.parent.push(n);
            self.size.push(1);
            n
        })
    }
    fn find(&mut self, x: &JunctionBox) -> usize {
        let mut i = *self.elts_to_ix.get(x).unwrap_or_else(|| panic!("Not inserted: {x:?}"));
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

fn part_1(boxes: &[JunctionBox]) -> usize {
    let mut circuits = Circuits::from(boxes);
    let mut pairs = boxes
        .iter()
        .tuple_combinations()
        .map(|(&a, &b)| Reverse(Pair::from((a, b))))
        .collect::<BinaryHeap<Reverse<Pair>>>();
    for i in 0..1_000 {
        let Reverse(p) = pairs.pop().unwrap_or_else(|| panic!("pair {i}"));
        circuits.union(&p.first, &p.second);
    }
    boxes
        .iter()
        .map(|x| circuits.parent[*circuits.elts_to_ix.get(x).expect("inserted")])
        .unique()
        .map(|i| circuits.size[i])
        .sorted()
        .rev()
        .take(3)
        .product()
}

fn main() {
    let input = std::fs::read_to_string("input.txt").expect("input");
    let boxes = parse(&input);
    println!("{}", part_1(&boxes));
}
