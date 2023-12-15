use indexmap::IndexMap;
use std::fs;

fn hash(s: &str) -> usize {
    s.chars().fold(0, |h, c| ((h + c as usize) * 17) % 256)
}

fn part1(input: &str) -> usize {
    input.split(',').map(hash).sum()
}

fn part2(s: &str) -> usize {
    let mut boxes = vec![IndexMap::<&str, usize>::new(); 256];
    for step in s.split(',') {
        let h = hash(step.trim_end_matches(|c: char| c == '-' || c == '=' || c.is_numeric()));
        if step.ends_with('-') {
            let _ = boxes[h].shift_remove(&step[0..step.len() - 1]);
        } else {
            let (l, n) = step.split_once("=").unwrap();
            boxes[h].insert(l, n.parse().unwrap());
        }
    }
    boxes
        .iter()
        .enumerate()
        .map(|(i, b)| {
            b.values()
                .enumerate()
                .map(|(j, l)| (i + 1) * (j + 1) * l)
                .sum::<usize>()
        })
        .sum()
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Should be able to read input");
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}
