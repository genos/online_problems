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
        Ok(match (1..1 + num / 2)
            .filter(|i| num % i == 0)
            .sum::<u64>()
            .cmp(&num)
        {
            Ordering::Less => Classification::Deficient,
            Ordering::Equal => Classification::Perfect,
            Ordering::Greater => Classification::Abundant,
        })
    }
}
