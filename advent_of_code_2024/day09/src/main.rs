use eyre::{Result, WrapErr};
use std::{fs, iter};

// with help from https://github.com/hasanghorbel/aoc-2024/tree/master/day9/src
fn part1(input: &str) -> usize {
    let mut blocks = input
        .chars()
        .filter(|&c| c.is_numeric())
        .enumerate()
        .flat_map(|(i, c)| {
            let d = c.to_digit(10).expect("is_numeric() should handle this.") as usize;
            iter::repeat_n(if i & 1 == 0 { Some(i / 2) } else { None }, d)
        })
        .collect::<Vec<_>>();
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
    blocks
        .iter()
        .enumerate()
        .filter_map(|(i, b)| b.map(|n| i * n))
        .sum()
}

// with help from https://github.com/nertsch/advent-of-code-2024/blob/master/src/day_09.rs
fn part2(input: &str) -> usize {
    let (mut files, mut frees, mut j) = (Vec::new(), Vec::new(), 0);
    for (i, c) in input.chars().filter(|&c| c.is_numeric()).enumerate() {
        let d = c.to_digit(10).expect("is_numeric() should handle this.") as usize;
        let r = j..j + d;
        if i & 1 == 0 {
            files.push((r, i / 2));
        } else {
            frees.push(r);
        }
        j += d;
    }
    for file in files.iter_mut().map(|(f, _)| f).rev() {
        if let Some((i, free)) = frees
            .iter_mut()
            .enumerate()
            .find(|(_, b)| b.end <= file.start && b.len() >= file.len())
        {
            let start = free.start;
            *free = start + file.len()..free.end;
            *file = start..start + file.len();
            if free.len() == 0 {
                frees.remove(i);
            }
        }
    }
    files.into_iter().map(|(r, d)| r.sum::<usize>() * d).sum()
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt").wrap_err("Unable to read input file.")?;
    println!("{}", part1(&input));
    println!("{}", part2(&input));
    Ok(())
}
