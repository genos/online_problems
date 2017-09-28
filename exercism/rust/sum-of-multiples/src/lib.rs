pub fn sum_of_multiples(m: u64, ns: &Vec<u64>) -> u64 {
    (0..m).filter(|k| ns.into_iter().any(|&n| k % n == 0)).sum()
}
