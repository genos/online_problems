use std::fs::File;
use std::io::prelude::*;

fn range(ns: &[u64]) -> u64 {
    if ns.is_empty() {
        panic!("empty range");
    } else {
        let mut i: u64 = std::u64::MAX;
        let mut x: u64 = std::u64::MIN;
        for &n in ns {
            if i > n {
                i = n;
            }
            if x < n {
                x = n;
            }
        }
        x - i
    }
}

fn sum_range(nss: &[Vec<u64>]) -> u64 {
    nss.iter().map(|ns| range(ns)).sum()
}

fn parse_input(s: &str) -> Vec<Vec<u64>> {
    s.lines()
        .map(|l| {
            l.split_whitespace()
                .map(|w| w.parse::<u64>())
                .filter_map(Result::ok)
                .collect()
        })
        .collect()
}

fn read_input() -> String {
    let mut f = File::open("../input.txt").expect("'input.txt' not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("unable to read 'input.txt'");
    contents
}

fn evenly_divide(ns: &[u64]) -> u64 {
    let l = ns.len();
    if l < 2 {
        panic!("evenly_divide with less than 2 elements");
    } else {
        for i in 0..l {
            let a = ns[i];
            for b in ns.iter().take(l).skip(i + 1) {
                if (a % b) == 0 {
                    return a / b;
                } else if (b % a) == 0 {
                    return b / a;
                }
            }
        }
        panic!("no even division");
    }
}

fn sum_evenly_divide(nss: &[Vec<u64>]) -> u64 {
    nss.iter().map(|ns| evenly_divide(ns)).sum()
}

fn main() {
    let input = parse_input(&read_input());
    let part1 = sum_range(&input);
    let part2 = sum_evenly_divide(&input);
    println!("{}", part1);
    println!("{}", part2);
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_range() {
        assert_eq!(range(&[5, 1, 9, 5]), 8);
        assert_eq!(range(&[7, 5, 3]), 4);
        assert_eq!(range(&[2, 4, 6, 8]), 6);
    }

    #[test]
    fn test_sum_range() {
        assert_eq!(
            sum_range(&[vec![5, 1, 9, 5], vec![7, 5, 3], vec![2, 4, 6, 8]]),
            18
        );
    }

    #[test]
    fn test_parse_input() {
        assert_eq!(
            parse_input("5 1 9 5\n7 5 3\n 2 4 6 8"),
            vec![vec![5, 1, 9, 5], vec![7, 5, 3], vec![2, 4, 6, 8]]
        );
    }

    #[test]
    fn test_evenly_divide() {
        assert_eq!(evenly_divide(&[5, 9, 2, 8]), 4);
        assert_eq!(evenly_divide(&[9, 4, 7, 3]), 3);
        assert_eq!(evenly_divide(&[3, 8, 6, 5]), 2);
    }

    #[test]
    fn test_sum_evenly_divide() {
        assert_eq!(
            sum_evenly_divide(&[vec![5, 9, 2, 8], vec![9, 4, 7, 3], vec![3, 8, 6, 5]]),
            9
        );
    }

}
