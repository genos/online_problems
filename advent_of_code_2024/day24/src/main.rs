use eyre::Result;
use std::{collections::BTreeMap, fs, str::FromStr};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Op {
    And,
    Or,
    Xor,
}

impl FromStr for Op {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "AND" => Ok(Self::And),
            "OR" => Ok(Self::Or),
            "XOR" => Ok(Self::Xor),
            _ => Err(format!("Bad op str: {s}")),
        }
    }
}

impl Op {
    fn app(self, x: u64, y: u64) -> u64 {
        match self {
            Self::And => x & y,
            Self::Or => x | y,
            Self::Xor => x ^ y,
        }
    }
}

peg::parser! {
    grammar parser() for str {
        pub rule parse() -> (BTreeMap<&'input str, u64>, Vec<(&'input str, Op, &'input str, &'input str)>) =
            i:initial() "\n\n" c:conns() "\n"? { (i, c) }
        rule initial() -> BTreeMap<&'input str, u64> = is:(ib() ++ "\n") { is.into_iter().collect() }
        rule conns() -> Vec<(&'input str, Op, &'input str, &'input str)> = c:(conn() ++ "\n")
        rule ib() -> (&'input str, u64) = i:input() ": " b:bit() { (i, b) }
        rule conn() -> (&'input str, Op, &'input str, &'input str) =
            x:name() " " o:op() " " y:name() " -> " z:name() { (x, o, y, z) }
        rule op() -> Op = o:$("AND" / "OR" / "XOR") {? o.parse().or(Err("op")) }
        rule input() -> &'input str = $(['x'|'y']['a'..='z'|'0'..='9']*<2>)
        rule name() -> &'input str = $(['a'..='z'|'0'..='9']*<3>)
        rule bit() -> u64 = n:$(['0'|'1']) {? n.parse().or(Err("num")) }
    }
}

fn part1(inputs: &BTreeMap<&str, u64>, connections: &[(&str, Op, &str, &str)]) -> u64 {
    let mut wires = inputs.clone();
    let mut cs = connections.to_vec();
    while !cs.is_empty() {
        cs.retain(|(x, op, y, z)| {
            if let (Some(&u), Some(&v)) = (wires.get(x), wires.get(y)) {
                wires.insert(z, op.app(u, v));
                false
            } else {
                true
            }
        });
    }
    wires
        .iter()
        .filter(|(k, _)| k.starts_with('z'))
        .rev()
        .fold(0, |x, (_, y)| (x << 1) | y)
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt").expect("Can't read file");
    let (inputs, connections) = parser::parse(&input)?;
    println!("{}", part1(&inputs, &connections));
    Ok(())
}
