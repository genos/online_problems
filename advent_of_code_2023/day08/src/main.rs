use eyre::{Result, WrapErr};
use std::{collections::BTreeMap, fs};

#[derive(Clone, Copy)]
enum Dir {
    L,
    R,
}

impl Dir {
    fn from(c: char) -> Self {
        match c {
            'L' => Self::L,
            'R' => Self::R,
            _ => panic!("Unexpected dir: {c}"),
        }
    }
}

type Pos = [char; 3];
type Network = BTreeMap<Pos, (Pos, Pos)>;

peg::parser! {
    grammar parser() for str {
        pub(super) rule map() -> (Vec<Dir>, Network) = d:(dirs()) "\n\n" n:(network()) { (d, n) }
        rule dirs() -> Vec<Dir> = ds:['L' | 'R']+ { ds.into_iter().map(Dir::from).collect() }
        rule network() -> Network = rs:(row() ** "\n") { rs.into_iter().collect() }
        rule row() -> (Pos, (Pos, Pos)) = p:pos() " = (" l:pos() ", " r:pos() ")" { (p, (l, r)) }
        rule pos() -> Pos = p:['A'..='Z']*<3> { [p[0], p[1], p[2]] }
    }
}

fn part1(dirs: Vec<Dir>, network: Network) -> u64 {
    let mut p = ['A', 'A', 'A'];
    let mut i = 0;
    let mut n = 0;
    while p != ['Z', 'Z', 'Z'] {
        let d = dirs[i];
        i = (i + 1) % (dirs.len());
        n += 1;
        if let Some(&(l, r)) = network.get(&p) {
            p = match d {
                Dir::L => l,
                Dir::R => r,
            }
        } else {
            panic!("Unknown position: {p:?}");
        }
    }
    n
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt").wrap_err("Unable to read input file.")?;
    let (dirs, network) = parser::map(input.trim()).wrap_err("Bad parse.")?;
    println!("{}", part1(dirs, network));
    Ok(())
}
