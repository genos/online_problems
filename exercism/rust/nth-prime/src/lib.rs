pub fn nth(n: usize) -> Result<usize, String> {
    if n == 0 {
        Err("For some reason we're 1-indexing…".to_string())
    } else if n == 1 {
        Ok(2)
    } else {
        let mut ps = vec![true; 1000000];
        ps[0] = false;
        ps[1] = false;
        ps[2] = true;
        let mut p = 2;
        while p < (ps.len() as f64).sqrt() as usize {
            let mut i = p + p;
            while i < ps.len() {
                ps[i] = false;
                i += p;
            }
            p = ps.iter()
                  .enumerate()
                  .filter(|&(j, b)| j > p && *b)
                  .next()
                  .unwrap()
                  .0;
        }
        (2..ps.len()).filter(|&i| ps[i])
                     .skip(n - 1)
                     .next()
                     .ok_or("Sorry, I failed.".to_string())
    }
}
