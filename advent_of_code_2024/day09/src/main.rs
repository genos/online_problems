use eyre::{Result, WrapErr};
use std::fs;

#[derive(Debug, Clone, Copy)]
enum Block {
    File(usize),
    Free,
}

impl Block {
    fn is_file(&self) -> bool {
        matches!(self, Self::File(_))
    }
    fn is_free(&self) -> bool {
        matches!(self, Self::Free)
    }
    fn get(&self) -> Option<usize> {
        match self {
            Self::File(n) => Some(*n),
            Self::Free => None,
        }
    }
}

fn parse(input: &str) -> Vec<Block> {
    let mut i = 0;
    let mut is_file = true;
    let mut output = vec![];
    for c in input.chars() {
        if c.is_numeric() {
            let d = c.to_digit(10).expect("is_numeric() should handle this.");
            let b = if is_file {
                i += 1;
                Block::File(i - 1)
            } else {
                Block::Free
            };
            for _ in 0..d {
                output.push(b);
            }
            is_file = !is_file;
        }
    }
    output
}

fn part1(mut blocks: Vec<Block>) -> usize {
    let (mut left, mut right) = (0, blocks.len() - 1);
    while left < right {
        while blocks[left].is_file() {
            left += 1;
        }
        while blocks[right].is_free() {
            right -= 1;
        }
        if left < right {
            blocks.swap(left, right);
        }
        left += 1;
        right -= 1;
    }
    blocks
        .into_iter()
        .enumerate()
        .filter_map(|(i, b)| b.get().map(|n| i * n))
        .sum()
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt").wrap_err("Unable to read input file.")?;
    let blocks = parse(&input);
    println!("{}", part1(blocks.clone()));
    Ok(())
}
