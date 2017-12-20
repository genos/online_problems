use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fs::File;
use std::io::prelude::*;

#[derive(PartialEq)]
enum Part {
    One,
    Two,
}

fn read_input() -> String {
    let mut f = File::open("../input.txt").expect("'input.txt' not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("unable to read 'input.txt'");
    contents
}

fn parse_input(input: &str) -> Vec<u64> {
    input
        .split_whitespace()
        .map(|w| w.trim().parse().expect("unable to parse number"))
        .collect()
}

fn update(banks: &mut [u64]) {
    let mut i = banks.len() - 1;
    let mut max_val = 0;
    for (j, val) in banks.iter().enumerate().rev() {
        if val >= &max_val {
            i = j;
            max_val = *val;
        }
    }
    banks[i] = 0;
    while max_val > 0 {
        i += 1;
        if i >= banks.len() {
            i = 0;
        }
        banks[i] += 1;
        max_val -= 1;
    }
}

fn cycle(input: &[u64], part: &Part) -> usize {
    let mut banks = input.to_owned();
    let mut seen = HashMap::new();
    let mut steps = 0;
    loop {
        match seen.entry(banks.clone()) {
            Entry::Occupied(ref entry) => {
                if part == &Part::One {
                    return steps;
                } else {
                    return (steps - entry.get()) as usize;
                }
            },
            Entry::Vacant(entry) => {
                entry.insert(steps);
                steps += 1;
                update(banks.as_mut_slice());
            },
        }
    }
}

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", cycle(&input, &Part::One));
    println!("Part 2: {}", cycle(&input, &Part::Two));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_update() {
        let mut banks = vec![0, 2, 7, 0];
        update(banks.as_mut_slice());
        assert_eq!(banks, vec![2, 4, 1, 2]);
        update(banks.as_mut_slice());
        assert_eq!(banks, vec![3, 1, 2, 3]);
        update(banks.as_mut_slice());
        assert_eq!(banks, vec![0, 2, 3, 4]);
        update(banks.as_mut_slice());
        assert_eq!(banks, vec![1, 3, 4, 1]);
        update(banks.as_mut_slice());
        assert_eq!(banks, vec![2, 4, 1, 2]);
    }

    #[test]
    fn test_cycle_one() {
        assert_eq!(cycle(&[0, 2, 7, 0], &Part::One), 5);
    }
    #[test]
    fn test_count_cycle() {
        assert_eq!(cycle(&[0, 2, 7, 0], &Part::Two), 4);
    }
}
