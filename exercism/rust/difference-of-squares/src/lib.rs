pub fn square_of_sum(n: u64) -> u64 {
    // https://en.wikipedia.org/wiki/Triangular_number
    let t = n * (n + 1) / 2;
    t * t
}

pub fn sum_of_squares(n: u64) -> u64 {
    // https://en.wikipedia.org/wiki/Square_pyramidal_number
    n * (n + 1) * (2 * n + 1) / 6
}

pub fn difference(n: u64) -> u64 {
    square_of_sum(n) - sum_of_squares(n)
}
