#[derive(Debug, PartialEq)]
pub enum LSPError {
    SizeBiggerThanDigitsLength,
    BadDigit,
    OtherNumericalProblem,
}

pub fn lsp(digits: &str, size: usize) -> Result<u32, LSPError> {
    if size > digits.len() {
        Err(LSPError::SizeBiggerThanDigitsLength)
    } else if size == 0 {
        Ok(1)
    } else {
        digits
            .chars()
            .map(|c| c.to_digit(10).ok_or(LSPError::BadDigit))
            .collect::<Result<Vec<u32>, LSPError>>()
            .and_then(|ds| {
                ds.windows(size)
                    .map(|w| w.iter().product())
                    .max()
                    .ok_or(LSPError::OtherNumericalProblem)
            })
    }
}
