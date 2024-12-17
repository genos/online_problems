use eyre::Result;
use rayon::prelude::*;
use std::fs;

#[derive(Clone)]
struct Computer {
    a: usize,
    b: usize,
    c: usize,
    ip: usize,
    ops: Vec<usize>,
}

peg::parser! {
    grammar parser() for str {
        pub rule computer() -> Computer =
            "Register A: " a:num()
            "\nRegister B: " b:num()
            "\nRegister C: " c:num()
            "\n\nProgram: " ops:(num() ++",") {
                Computer { a, b, c, ip: 0, ops }
            }
        rule num() -> usize = n:$(['0'..='9']['0'..='9']*) {? n.parse().or(Err("num")) }
    }
}

impl Computer {
    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn combo(&self, w: usize) -> usize {
        match w {
            4 => self.a,
            5 => self.b,
            6 => self.c,
            _ => w, // pass through invalid states, too
        }
    }
    fn run(&mut self) -> Vec<usize> {
        let mut output = vec![];
        while self.ip < self.ops.len() {
            // safety checked above
            let code = unsafe { *self.ops.get_unchecked(self.ip) };
            let word = unsafe { *self.ops.get_unchecked(self.ip + 1) };
            match code {
                0 => self.a /= 1 << self.combo(word),
                1 => self.b ^= word,
                2 => self.b = self.combo(word) % 8,
                3 => {
                    if self.a != 0 {
                        self.ip = word;
                        continue;
                    }
                }
                4 => self.b ^= self.c,
                5 => {
                    #[allow(clippy::cast_possible_truncation)]
                    output.push(self.combo(word) % 8);
                }
                6 => self.b = self.a / (1 << self.combo(word)),
                7 => self.c = self.a / (1 << self.combo(word)),
                _ => {} // invalid, pass through
            }
            self.ip += 2;
        }
        output
    }
    fn part1(&mut self) {
        let mut sep = "";
        for x in &self.run() {
            print!("{sep}{x}");
            sep = ",";
        }
        println!();
    }
    fn set_a(&mut self, a: usize) -> &mut Self {
        self.a = a;
        self
    }
    fn part2(&self) {
        println!(
            "{:?}",
            (0..usize::MAX)
                .into_par_iter()
                .by_exponential_blocks()
                .find_first(|&i| self.clone().set_a(i).run() == self.ops)
        );
    }
}

fn main() -> Result<()> {
    let input = fs::read_to_string("input.txt")?;
    let mut c = parser::computer(&input)?;
    c.part1();
    c.ip = 0;
    c.part2();
    Ok(())
}
