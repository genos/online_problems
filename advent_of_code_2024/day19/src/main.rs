use eyre::Result;
use rayon::prelude::*;
use fxhash::FxHashSet;
use std::fs;

struct Problem<'a> {
    available: FxHashSet<&'a str>,
    designs: Vec<&'a str>,
}

peg::parser! {
    grammar parser() for str {
        pub rule problem() -> Problem<'input> = available:av() "\n\n" designs:ds() { Problem { available, designs } }
        rule av() -> FxHashSet<&'input str> = ts:(towel() ++ ", ") { ts.into_iter().collect() }
        rule ds() -> Vec<&'input str> = towel() ++ "\n"
        rule towel() -> &'input str = $(['w' | 'u' | 'b' | 'r' | 'g']+)
    }
}

impl Problem<'_> {
    // dynamic programing: word break count
    fn wbc(&self, d: &str) -> u64 {
        let mut dp = vec![0; d.len() + 1];
        dp[0] = 1;
        for i in 0..=d.len() {
            for j in 0..i {
                if self.available.contains(&d[j..i]) {
                    dp[i] += dp[j];
                }
            }
        }
        dp[d.len()]
    }
    fn part1(&self) -> usize {
        self.designs
            .par_iter()
            .filter(|d| self.wbc(d) > 0)
            .count()
    }
    fn part2(&self) -> u64 {
        self.designs
            .par_iter()
            .map(|d| self.wbc(d))
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
