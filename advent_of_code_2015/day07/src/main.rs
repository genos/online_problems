use std::error::Error;
use std::fs::File;
use std::io::BufReader;
use std::io::Lines;
use std::io::prelude::*;
use std::path::Path;

fn input() -> Lines<BufReader<File>> {
    let p = Path::new("data/input.txt");
    match File::open(&p) {
        Err(e) => panic!("Couldn't open {}: {}", p.display(), e.description()),
        Ok(f) => BufReader::new(f).lines(),
    }
}

fn main() {
    let i: Vec<_> = input().collect();
    println!("{}", i.len());
}
