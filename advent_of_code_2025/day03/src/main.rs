fn digits(s: &str) -> Vec<Vec<u32>> {
    s.trim()
        .lines()
        .map(|l| l.chars().map(|c| c.to_digit(10).expect("digit")).collect())
        .collect()
}

fn part_1(banks: &[Vec<u32>]) -> u32 {
    banks
        .iter()
        .map(|bank| {
            (0..bank.len())
                .flat_map(|i| (i + 1..bank.len()).map(move |j| 10 * bank[i] + bank[j]))
                .max()
                .expect("nums")
        })
        .sum()
}

fn main() {
    let input = digits(&std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", part_1(&input));
}
