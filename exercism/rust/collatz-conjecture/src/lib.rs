#[derive(Debug, PartialEq)]
pub enum CollatzError {
    ZeroIsNotAValidStartingPointYouSillyPerson,
}

pub fn collatz(n: u64) -> Result<u64, CollatzError> {
    if n == 0 {
        Err(CollatzError::ZeroIsNotAValidStartingPointYouSillyPerson)
    } else {
        let mut m: u64 = n;
        let mut c: u64 = 0;
        while m > 1 {
            c += 1;
            m = if m & 1 == 0 { m / 2 } else { 3 * m + 1 } ;
        }
        Ok(c)
    }
}
