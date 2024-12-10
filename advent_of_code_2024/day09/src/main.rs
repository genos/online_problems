// https://github.com/hasanghorbel/aoc-2024/tree/master/day9/src assistance
use eyre::{Result, WrapErr};
use std::fs;

fn checksum(blocks: impl Iterator<Item = Option<usize>>) -> usize {
    blocks
        .enumerate()
        .filter_map(|(i, b)| b.map(|n| i * n))
        .sum()
}

fn parse1(input: &str) -> Vec<Option<usize>> {
    let (mut i, mut is_file, mut output) = (0, true, vec![]);
    for c in input.chars() {
        if c.is_numeric() {
            let d = c.to_digit(10).expect("is_numeric() should handle this.") as usize;
            let to_add = if is_file {
                i += 1;
                Some(i - 1)
            } else {
                None
            };
            output.extend(vec![to_add; d]);
            is_file = !is_file;
        }
    }
    output
}

fn part1(input: &str) -> usize {
    let mut blocks = parse1(input);
    let (mut left, mut right) = (0, blocks.len() - 1);
    while left < right {
        while blocks[left].is_some() {
            left += 1;
        }
        while blocks[right].is_none() {
            right -= 1;
        }
        if left < right {
            blocks.swap(left, right);
        }
        (left, right) = (left + 1, right - 1);
    }
    checksum(blocks.into_iter())
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt").wrap_err("Unable to read input file.")?;
    println!("{}", part1(&input));
    Ok(())
}
