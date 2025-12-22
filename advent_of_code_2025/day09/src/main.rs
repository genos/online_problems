use itertools::Itertools;

type Tile = (usize, usize);
type Pair = (Tile, Tile);
type Bounds = (usize, usize, usize, usize);
type Area = usize;

fn parse(s: &str) -> Vec<Tile> {
    s.trim()
        .lines()
        .map(|line| {
            let (x, y) = line.split_once(',').expect("comma");
            (x.parse().expect("x"), y.parse().expect("y"))
        })
        .collect()
}

fn all_pairs(tiles: &[Tile]) -> impl Iterator<Item = Pair> {
    tiles.iter().copied().tuple_combinations()
}

fn area(((x1, y1), (x2, y2)): Pair) -> Area {
    (x1.abs_diff(x2) + 1) * (y1.abs_diff(y2) + 1)
}

fn part_1(tiles: &[Tile]) -> Area {
    all_pairs(tiles).map(area).max().unwrap_or_default()
}

fn to_bounds(((x1, y1), (x2, y2)): Pair) -> Bounds {
    (x1.min(x2), x1.max(x2), y1.min(y2), y1.max(y2))
}

// with help from https://www.reddit.com/r/adventofcode/comments/1pibab2/comment/nt50hqt
fn part_2(tiles: &[(usize, usize)]) -> Area {
    let bounds = tiles
        .iter()
        .copied()
        .chain(std::iter::once(tiles[0]))
        .tuple_windows()
        .map(to_bounds)
        .collect::<Vec<_>>();
    all_pairs(tiles)
        .filter(|pair| {
            let (x_lo, x_hi, y_lo, y_hi) = to_bounds(*pair);
            !bounds.iter().any(|&(h_lo, h_hi, v_lo, v_hi)| {
                let vertical = (h_lo == h_hi)
                    && (x_lo + 1..x_hi).contains(&h_lo)
                    && ((v_lo..v_hi).contains(&y_lo) || (v_lo + 1..v_hi + 1).contains(&y_hi));
                let horizontal = (v_lo == v_hi)
                    && (y_lo + 1..y_hi).contains(&v_lo)
                    && ((h_lo..h_hi).contains(&x_lo) || (h_lo + 1..h_hi + 1).contains(&x_hi));
                vertical || horizontal
            })
        })
        .map(area)
        .max()
        .unwrap_or_default()
}

fn main() {
    let input = parse(
        &std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/input.txt")).expect("file"),
    );
    println!("{}", part_1(&input));
    println!("{}", part_2(&input));
}
