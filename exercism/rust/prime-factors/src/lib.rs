pub fn factors(n: u64) -> Vec<u64> {
    let mut fs = vec![];
    let mut m = n;
    while 0 == m & 1 {
        m >>= 1;
        fs.push(2);
    }
    let mut i = 3;
    while m > 1 {
        while 0 == m % i {
            m /= i;
            fs.push(i);
        }
        i += 2;
    }
    fs
}
