use std::fs::File;
use std::io::prelude::*;

fn read_input() -> String {
    let mut f = File::open("input.txt").expect("'input.txt' not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("unable to read 'input.txt'");
    contents
}

fn parse_input(input: &str) -> Vec<i64> {
    input
        .lines()
        .map(|l| l.trim().parse().expect("unable to parse number"))
        .collect()
}

fn run(input: &[i64], f: fn(i64) -> i64) -> usize {
    let mut instructions = input.to_owned();
    let mut steps = 0;
    let mut pointer = 0i64;
    let len = instructions.len() as i64;
    while 0 <= pointer && pointer < len {
        steps += 1;
        let ptr = pointer.abs() as usize;
        let offset = instructions[ptr];
        instructions[ptr] += f(offset);
        pointer += offset;
    }
    steps
}

fn part1(input: &[i64]) -> usize {
    run(input, |_| 1)
}

fn part2(input: &[i64]) -> usize {
    run(input, |offset| if offset >= 3 { -1 } else { 1 })
}

fn main() {
    let input = parse_input(&read_input());
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}
