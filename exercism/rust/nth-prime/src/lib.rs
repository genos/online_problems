#[derive(Debug, PartialEq)]
pub enum NthPrimeError {
    WhyAreWeIndexingFromOneInsteadOfZero,
    IveMadeATerribleMistake,
}

pub fn nth(n: usize) -> Result<usize, NthPrimeError> {
    if n == 0 {
        Err(NthPrimeError::WhyAreWeIndexingFromOneInsteadOfZero)
    } else {
        let mut ps = vec![2];
        let mut q = 3;
        while ps.len() < n {
            if ps.iter().all(|p| q % p != 0) {
                ps.push(q);
            }
            q += 2;
        }
        ps.pop().ok_or(NthPrimeError::IveMadeATerribleMistake)
    }
}
