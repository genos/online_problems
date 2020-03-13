fn from_str(s: &str) -> Vec<i64> {
    let mut out = Vec::new();
    for c in s.chars() {
        out.push(c.to_digit(10).expect("Bad digit") as i64);
    }
    out
}

fn horner(list: &[i64]) -> i64 {
    list.iter().fold(0, |total, &i| total * 10 + i)
}

fn base(i: usize, round: usize) -> i64 {
    [0, 1, 0, -1][((i + 1) / (round + 1)) % 4]
}

fn phase(list: &[i64]) -> Vec<i64> {
    let mut out = vec![0; list.len()];
    for (round, d) in out.iter_mut().enumerate() {
        for (i, x) in list.iter().enumerate() {
            *d += x * base(i, round);
        }
        *d = d.abs() % 10;
    }
    out
}

fn part1(list: &[i64]) -> i64 {
    let mut scratch = Vec::from(list);
    for _ in 0..100 {
        scratch = phase(&scratch);
    }
    horner(&scratch[..8])
}

fn part2(list: &[i64]) -> i64 {
    let repeat = 10000;
    let offset = horner(&list[..7]) as usize;
    let mut scratch = list
        .iter()
        .cycle()
        .take(list.len() * repeat)
        .copied()
        .collect::<Vec<_>>();
    if offset < list.len() * repeat / 2 {
        println!("going slow…");
        for _ in 0..100 {
            scratch = phase(&scratch);
        }
        horner(&scratch[offset..(offset + 8)])
    } else {
        println!("going fast!");
        // https://github.com/enjmusic/aoc_2019/blob/master/aoc_16/src/main.rs
        // Because of how the patterns work for the 2nd half of the values, everything under the
        // diagonal of the transformation matrix is a one. So, for indices past halfway their next
        // FFT is just the last digit of the sum of all digits after them. We can construct each
        // one of these in O(n) time by maintaining a partial sum over time.
        scratch = (offset..(list.len() * repeat))
            .map(|i| list[i % list.len()])
            .collect::<Vec<_>>();
        for _ in 0..100 {
            let mut partial = 0;
            for i in (0..scratch.len()).rev() {
                partial += scratch[i];
                scratch[i] = partial.abs() % 10;
            }
        }
        horner(&scratch[..8])
    }
}

fn main() {
    let input = from_str(&std::fs::read_to_string("input").expect("Unable to read input file"));
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_base() {
        assert_eq!(
            (0..8).map(|i| base(i, 0)).collect::<Vec<_>>(),
            [1, 0, -1, 0, 1, 0, -1, 0]
        );
        assert_eq!(
            (0..8).map(|i| base(i, 1)).collect::<Vec<_>>(),
            [0, 1, 1, 0, 0, -1, -1, 0]
        );
        assert_eq!(
            (0..8).map(|i| base(i, 2)).collect::<Vec<_>>(),
            [0, 0, 1, 1, 1, 0, 0, 0]
        );
        assert_eq!(
            (0..8).map(|i| base(i, 3)).collect::<Vec<_>>(),
            [0, 0, 0, 1, 1, 1, 1, 0]
        );
        assert_eq!(
            (0..8).map(|i| base(i, 4)).collect::<Vec<_>>(),
            [0, 0, 0, 0, 1, 1, 1, 1]
        );
    }
    #[test]
    fn test_horner() {
        assert_eq!(12345678, horner(&from_str("12345678")));
        assert_eq!(5678, horner(&from_str("12345678")[4..]));
    }
    #[test]
    fn test_phase() {
        let mut xs = from_str("12345678");
        xs = phase(&xs);
        assert_eq!(xs, from_str("48226158"));
        xs = phase(&xs);
        assert_eq!(xs, from_str("34040438"));
        xs = phase(&xs);
        assert_eq!(xs, from_str("03415518"));
    }
    #[test]
    fn test_part1() {
        assert_eq!(
            part1(&from_str("80871224585914546619083218645595")),
            24176176
        );
        assert_eq!(
            part1(&from_str("19617804207202209144916044189917")),
            73745418
        );
        assert_eq!(
            part1(&from_str("69317163492948606335995924319873")),
            52432133
        );
    }
}
