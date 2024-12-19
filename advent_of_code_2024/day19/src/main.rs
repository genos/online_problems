use eyre::Result;
use std::{collections::HashSet, fs};

#[derive(Debug)]
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
    // dynamic programing word break
    fn possible(&self, d: &str) -> bool {
        let mut dp = vec![false; d.len() + 1];
        dp[0] = true;
        for i in 0..d.len() + 1 {
            for j in 0..i {
                if dp[j] && self.available.contains(&d[j..i]) {
                    dp[i] = true;
                    break;
                }
            }
        }
        dp[d.len()]
    }
    fn part1(&self) -> usize {
        self.designs.iter().filter(|d| self.possible(d)).count()
    }
    // redo possible, but counting
    fn all_possible(&self, d: &str) -> usize {
        let mut dp = vec![0; d.len() + 1];
        dp[0] = 1;
        for i in 0..d.len() + 1 {
            for j in 0..i {
                if self.available.contains(&d[j..i]) {
                    dp[i] += dp[j];
                }
            }
        }
        dp[d.len()]
    }
    fn part2(&self) -> usize {
        self.designs.iter().map(|d| self.all_possible(d)).sum()
    }
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let p = parser::problem(&input)?;
    println!("{}", p.part1());
    println!("{}", p.part2());
    Ok(())
}
