use std::cmp::Ordering;

#[derive(PartialEq, Debug)]
pub enum Classification {
    Deficient,
    Perfect,
    Abundant,
}

pub fn classify(num: u64) -> Result<Classification, &'static str> {
    if num == 0 {
        Err("Number must be positive")
    } else {
        match (1..1 + num / 2)
            .filter(|i| num % i == 0)
            .sum::<u64>()
            .cmp(&num)
        {
            Ordering::Less => Ok(Classification::Deficient),
            Ordering::Equal => Ok(Classification::Perfect),
            Ordering::Greater => Ok(Classification::Abundant),
        }
    }
}
