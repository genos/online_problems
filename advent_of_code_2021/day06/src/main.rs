use anyhow::Result;
use std::str::FromStr;

fn parse_fish(name: &str) -> Result<Vec<u64>> {
    let raw = std::fs::read_to_string(name)?;
    let mut fish = vec![0; 9];
    for s in raw.split(',') {
        let i = usize::from_str(s)?;
        fish[i] += 1;
    }
    Ok(fish)
}

fn step(fish: &mut [u64]) {
    let fish_0 = fish[0];
    for i in 1..9 {
        fish[i - 1] = fish[i];
    }
    fish[8] = fish_0;
    fish[6] += fish_0;
}

fn lantern_fish(fish: &[u64], steps: u64) -> u64 {
    let mut new = [0; 9];
    new[..].clone_from_slice(fish);
    for _ in 0..steps {
        step(&mut new);
    }
    new.iter().sum()
}

fn main() {
    let fish = parse_fish("input.txt").expect("Error in parsing.");
    println!("{}", lantern_fish(&fish, 80));
    println!("{}", lantern_fish(&fish, 256));
}
