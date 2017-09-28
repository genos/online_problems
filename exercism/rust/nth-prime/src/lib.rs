pub fn nth(n: usize) -> Result<usize, String> {
    if n == 0 {
        Err("For some reason we're 1-indexing…".to_string())
    } else {
        let mut ps = vec![2];
        let mut q = 3;
        while ps.len() < n {
            if ps.iter().all(|p| q % p != 0) {
                ps.push(q);
            }
            q += 2;
        }
        ps.last().map(|&p| p).ok_or("OH NO".to_string())
    }
}
