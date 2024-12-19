use eyre::Result;
use std::{collections::HashSet, fs};

struct Problem<'a> {
    available: HashSet<&'a str>,
    designs: Vec<&'a str>,
}

peg::parser! {
    grammar parser() for str {
        pub rule problem() -> Problem<'input> = available:av() "\n\n" designs:ds() { Problem { available, designs } }
        rule av() -> HashSet<&'input str> = ts:(towel() ++ ", ") { ts.into_iter().collect() }
        rule ds() -> Vec<&'input str> = towel() ++ "\n"
        rule towel() -> &'input str = $(['w' | 'u' | 'b' | 'r' | 'g']+)
    }
}

impl Problem<'_> {
    // generic dynamic programing word break
    fn word_break<T: Copy>(&self, d: &str, zero: T, one: T, f: impl Fn(T, T) -> T) -> T {
        let mut dp = vec![zero; d.len() + 1];
        dp[0] = one;
        for i in 0..=d.len() {
            for j in 0..i {
                if self.available.contains(&d[j..i]) {
                    dp[i] = f(dp[i], dp[j]);
                }
            }
        }
        dp[d.len()]
    }
    fn part1(&self) -> usize {
        self.designs
            .iter()
            .filter(|d| self.word_break(d, false, true, |a, b| a | b))
            .count()
    }
    fn part2(&self) -> usize {
        self.designs
            .iter()
            .map(|d| self.word_break(d, 0, 1, |a, b| a + b))
            .sum()
    }
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let p = parser::problem(&input)?;
    println!("{}", p.part1());
    println!("{}", p.part2());
    Ok(())
}
