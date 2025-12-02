use std::fs;

fn read(s: &str) -> Vec<(u64, u64)> {
    parser::id_ranges(s).expect("Parse IDRanges")
}

peg::parser! {
    grammar parser() for str {
        #[no_eof] pub rule id_ranges() -> Vec<(u64, u64)> = r:id_range() ++ "," { r }
        rule id_range() -> (u64, u64) = lo:(num()) "-" hi:(num()) { (lo, hi) }
        rule num() -> u64 = n:$(['0'..='9']+) { n.parse().expect("Read u64") }
    }
}

fn digits(n: u64) -> Vec<u8> {
    let mut ds = vec![];
    let mut k = n;
    while k > 0 {
        ds.push((k % 10) as u8);
        k /= 10;
    }
    ds.reverse();
    ds
}

fn repeats<T: PartialEq>(xs: &[T]) -> bool {
    if xs.is_empty() {
        true
    } else {
        let n = xs.len() / 2;
        let ys = &xs[..n];
        let zs = &xs[n..];
        ys == zs
    }
}

fn part1_invalid(lo: u64, hi: u64) -> u64 {
    (lo..=hi).filter(|n| repeats(&digits(*n))).sum()
}

fn main() {
    println!(
        "{}",
        read(&fs::read_to_string("input.txt").expect("Read the file"))
            .into_iter()
            .map(|(lo, hi)| part1_invalid(lo, hi))
            .sum::<u64>()
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn digits_ok() {
        assert_eq!(digits(1234), vec![1, 2, 3, 4]);
        assert_eq!(digits(1), vec![1]);
        assert_eq!(digits(0), vec![]);
    }

    #[test]
    fn repeats_ok() {
        assert!(repeats(&digits(55)));
        assert!(repeats(&digits(12341234)));
    }

    #[test]
    fn part1_ok() {
        assert_eq!(part1_invalid(11, 22), 33);
        assert_eq!(part1_invalid(38593856, 38593862), 38593859);
    }
}
