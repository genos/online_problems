pub fn find() -> Option<u32> {
    let n: u32 = 1000;
    for a in 1..n {
        for b in 1..a - 1 {
            let c = n - a - b;
            if a * a + b * b == c * c {
                return Some(a * b * c);
            }
        }
    }
    None
}
