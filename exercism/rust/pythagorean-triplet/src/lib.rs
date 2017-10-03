pub fn find() -> Option<i32> {
    let n: i32 = 1000;
    for a in 1..n {
        for b in a..n {
            let c = n - a - b;
            if a * a + b * b == c * c {
                return Some(a * b * c);
            }
        }
    }
    None
}
