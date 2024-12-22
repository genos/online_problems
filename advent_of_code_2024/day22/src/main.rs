use rayon::prelude::*;
use std::fs;

#[derive(Debug)]
struct Secret(i64);

impl Iterator for Secret {
    type Item = i64;
    fn next(&mut self) -> Option<Self::Item> {
        self.0 = (self.0 ^ (self.0 << 6)) % 16_777_216;
        self.0 = (self.0 ^ (self.0 >> 5)) % 16_777_216;
        self.0 = (self.0 ^ (self.0 << 11)) % 16_777_216;
        Some(self.0)
    }
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Can't read file");
    let s = input
        .lines()
        .par_bridge()
        .map(|l| {
            Secret(l.parse().expect("Can't parse line"))
                .nth(1999)
                .unwrap_or_default()
        })
        .sum::<i64>();
    println!("{s}");
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn example() {
        assert_eq!(
            Secret(123).take(10).collect::<Vec<_>>(),
            [
                15_887_950, 16_495_136, 527_345, 704_524, 1_553_684, 12_683_156, 11_100_544,
                12_249_484, 7_753_432, 5_908_254
            ]
        );
    }
}
