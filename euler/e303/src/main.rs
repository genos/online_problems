use std::collections::VecDeque;

fn f_div_n(n: usize) -> usize {
    let mut queue = (1..=2).collect::<VecDeque<_>>();
    let mut seen = vec![false; n];
    while let Some(k) = queue.pop_front() {
        if k % n == 0 {
            return k / n;
        }
        for i in 0..=2 {
            let j = 10 * k + i;
            if !seen[j % n] {
                seen[j % n] = true;
                queue.push_back(j);
            }
        }
    }
    panic!("Empty queue for {n}")
}

fn main() {
    let e303 = (1..=10_000).map(f_div_n).sum::<usize>();
    println!("{e303}");
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_f_div_n() {
        assert_eq!(2 / 2, f_div_n(2));
        assert_eq!(12 / 3, f_div_n(3));
        assert_eq!(21 / 7, f_div_n(7));
        assert_eq!(210 / 42, f_div_n(42));
        assert_eq!(1_121_222 / 89, f_div_n(89));
        assert_eq!(12_222 / 9, f_div_n(9));
    }

    #[test]
    fn test_sum_f_div_n() {
        assert_eq!(11_363_107usize, (1..=100).map(f_div_n).sum());
    }
}
