use eyre::Result;
use std::{collections::HashSet, fs};

struct Problem {
    available: HashSet<String>,
    designs: Vec<String>,
}

peg::parser! {
    grammar parser() for str {
        pub rule problem() -> Problem = available:av() "\n\n" designs:ds() { Problem { available, designs } }
        rule av() -> HashSet<String> = ts:(towel() ++ ", ") { ts.into_iter().collect() }
        rule ds() -> Vec<String> = towel() ++ "\n"
        rule towel() -> String = t:$(['w' | 'u' | 'b' | 'r' | 'g']+) { t.to_string() }
    }
}

impl Problem {
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
    fn is_possible(&self, d: &str) -> bool {
        self.word_break(d, false, true, |a, b| a | b)
    }
    fn part1(&self) -> usize {
        self.designs.iter().filter(|d| self.is_possible(d)).count()
    }
    // redo is_possible, but counting
    fn count_possible(&self, d: &str) -> usize {
        self.word_break(d, 0, 1, |a, b| a + b)
    }
    fn part2(&self) -> usize {
        self.designs.iter().map(|d| self.count_possible(d)).sum()
    }
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let p = parser::problem(&input)?;
    println!("{}", p.part1());
    println!("{}", p.part2());
    Ok(())
}
