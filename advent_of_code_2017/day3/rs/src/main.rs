use std::collections::HashMap;

#[derive(Debug, PartialEq)]
enum Direction {
    R,
    U,
    L,
    D,
}

impl Direction {
    fn new() -> Direction {
        Direction::R
    }
    fn next(&self) -> Direction {
        match *self {
            Direction::R => Direction::U,
            Direction::U => Direction::L,
            Direction::L => Direction::D,
            Direction::D => Direction::R,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct Position {
    number: u64,
    coordinates: (i64, i64),
}

impl Position {
    fn new() -> Position {
        Position {
            number: 1u64,
            coordinates: (0i64, 0i64),
        }
    }
    fn dist(&self) -> i64 {
        self.coordinates.0.abs() + self.coordinates.1.abs()
    }
    fn step(&mut self, direction: &Direction) {
        self.number += 1;
        match *direction {
            Direction::R => self.coordinates.0 += 1,
            Direction::U => self.coordinates.1 += 1,
            Direction::L => self.coordinates.0 -= 1,
            Direction::D => self.coordinates.1 -= 1,
        }
    }
}

#[derive(Debug)]
struct Walker {
    position: Position,
    direction: Direction,
    steps_left_in_dir: u64,
    steps_in_next_dir: u64,
}

impl Walker {
    fn new() -> Walker {
        Walker {
            position: Position::new(),
            direction: Direction::new(),
            steps_left_in_dir: 1u64,
            steps_in_next_dir: 1u64,
        }
    }
    fn update(&mut self) {
        self.position.step(&self.direction);
        if self.steps_left_in_dir > 1 {
            self.steps_left_in_dir -= 1;
        } else {
            self.direction = self.direction.next();
            if self.direction == Direction::R || self.direction == Direction::L {
                self.steps_in_next_dir += 1;
            }
            self.steps_left_in_dir = self.steps_in_next_dir;
        }
    }
}

impl Iterator for Walker {
    type Item = Position;
    fn next(&mut self) -> Option<Position> {
        let p = self.position;
        self.update();
        Some(p)
    }
}

fn part1(n: usize) -> i64 {
    Walker::new()
        .nth(n - 1)
        .expect("What ⁇ Walkers are infinite iterators‼")
        .dist()
}

fn neighbors((a, b): (i64, i64)) -> Vec<(i64, i64)> {
    let mut ns = Vec::new();
    for i in &[-1, 0, 1] {
        for j in &[-1, 0, 1] {
            if (i, j) != (&0, &0) {
                ns.push((a + i, b + j));
            }
        }
    }
    ns
}

fn part2(n: usize) -> i64 {
    let mut h = HashMap::new();
    h.insert((0, 0), 1);
    let mut last: i64;
    for p in Walker::new().skip(1) {
        let cs = p.coordinates;
        last = neighbors(cs).iter().map(|cs| h.get(cs).unwrap_or(&0)).sum();
        h.insert(cs, last);
        if (last as usize) > n {
            return last;
        }
    }
    0
}

fn main() {
    let input = 277_678;
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_p() {
        let p = Position::new();
        assert_eq!(p.number, 1);
        assert_eq!(p.coordinates, (0, 0));
        assert_eq!(p.dist(), 0);
    }

    #[test]
    fn test_new_w() {
        let w = Walker::new();
        assert_eq!(w.position.number, 1);
        assert_eq!(w.position.coordinates, (0, 0));
        assert_eq!(w.direction, Direction::R);
        assert_eq!(w.steps_left_in_dir, 1);
        assert_eq!(w.steps_in_next_dir, 1);
    }

    #[test]
    fn test_iter_w() {
        let ps: Vec<Position> = Walker::new().take(10).collect();
        assert_eq!(
            ps.iter().map(|p| p.number).collect::<Vec<_>>(),
            (1..11).collect::<Vec<_>>()
        );
        assert_eq!(
            ps.iter().map(|p| p.coordinates).collect::<Vec<_>>(),
            vec![
                (0, 0),
                (1, 0),
                (1, 1),
                (0, 1),
                (-1, 1),
                (-1, 0),
                (-1, -1),
                (0, -1),
                (1, -1),
                (2, -1),
            ]
        );
        assert_eq!(
            ps.iter().map(|p| p.dist()).collect::<Vec<_>>(),
            vec![0, 1, 2, 1, 2, 1, 2, 1, 2, 3]
        );
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(1), 0);
        assert_eq!(part1(12), 3);
        assert_eq!(part1(23), 2);
        assert_eq!(part1(1024), 31);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(0), 1);
        assert_eq!(part2(1), 2);
        assert_eq!(part2(3), 4);
        assert_eq!(part2(4), 5);
        assert_eq!(part2(24), 25);
        assert_eq!(part2(800), 806);
    }
}
