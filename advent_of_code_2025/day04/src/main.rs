type Grid = Vec<Vec<bool>>; // true == paper

fn parse(s: &str) -> Grid {
    s.trim()
        .lines()
        .map(|line| line.trim().chars().map(|c| c == '@').collect())
        .collect()
}

#[allow(clippy::inline_always)]
#[inline(always)]
fn is_removable(i: usize, j: usize, g: &Grid) -> bool {
    let mut paper = 0u8;
    for x in i.saturating_sub(1)..=i.saturating_add(1).min(g.len().saturating_sub(1)) {
        for y in j.saturating_sub(1)
            ..=j.saturating_add(1)
                .min(unsafe { g.get_unchecked(i) }.len().saturating_sub(1))
        {
            if (x, y) != (i, j) && unsafe { *g.get_unchecked(x).get_unchecked(y) } {
                paper += 1;
            }
        }
    }
    paper < 4
}

#[allow(dead_code)] // See, we _can_ use safe Rust.
fn part_1_nice(g: &Grid) -> usize {
    g.iter()
        .enumerate()
        .map(|(i, row)| {
            row.iter()
                .enumerate()
                .filter(|&(j, &t)| t && is_removable(i, j, g))
                .count()
        })
        .sum()
}

fn part_1(g: &Grid) -> usize {
    let mut count = 0;
    for i in 0..g.len() {
        for j in 0..unsafe { g.get_unchecked(i) }.len() {
            if unsafe { *g.get_unchecked(i).get_unchecked(j) } && is_removable(i, j, g) {
                count += 1;
            }
        }
    }
    count
}

fn part_2(mut g: Grid) -> usize {
    let (mut removed, mut to_remove) = (0, Vec::with_capacity(g.len() * g.len()));
    loop {
        for i in 0..g.len() {
            for j in 0..unsafe { g.get_unchecked(i) }.len() {
                if unsafe { *g.get_unchecked(i).get_unchecked(j) } && is_removable(i, j, &g) {
                    to_remove.push((i, j));
                }
            }
        }
        if to_remove.is_empty() {
            break;
        }
        while let Some((i, j)) = to_remove.pop() {
            unsafe {
                *g.get_unchecked_mut(i).get_unchecked_mut(j) = false;
            }
            removed += 1;
        }
    }
    removed
}

fn main() {
    let input = parse(include_str!("../input.txt"));
    println!("{}", part_1(&input));
    println!("{}", part_2(input));
}
