use good_lp::{Expression, Solution, SolverModel, constraint, highs, variables};
use std::collections::{HashSet, VecDeque};

struct Machine {
    lights: Vec<bool>,
    wiring: Vec<Vec<u16>>,
    joltage: Vec<u16>,
}

peg::parser! {
    grammar parse() for str {
        pub rule it() -> Vec<Machine> = m() ** "\n"
        rule m() -> Machine = lights:l() _ wiring:ws() _ joltage:j() { Machine { lights, wiring, joltage } }
        rule _() = quiet!{[' '|'\t']*}
        rule l() -> Vec<bool> = "[" l:$(['.'|'#']*) "]" { l.chars().map(|c| c == '#').collect() }
        rule ws() -> Vec<Vec<u16>> = w() ** " "
        rule j() -> Vec<u16> = "{" ns:(n() ** ",") "}" { ns }
        rule w() -> Vec<u16> = "(" ns:(n() ** ",") ")" { ns }
        rule n() -> u16 = n:$(['0'..='9']+) { n.parse().expect("num") }
    }
}

impl Machine {
    fn part_1(&self) -> u16 {
        let (mut seen, mut todo) = (HashSet::new(), VecDeque::from([(self.lights.clone(), 0)]));
        while let Some((lights, steps)) = todo.pop_front() {
            if !lights.iter().any(|&l| l) {
                return steps;
            } else if seen.insert(lights.clone()) {
                for wires in &self.wiring {
                    let mut ls = lights.clone();
                    for w in wires {
                        ls[*w as usize] ^= true;
                    }
                    todo.push_back((ls, steps + 1));
                }
            }
        }
        unreachable!("part_1")
    }
}

fn part_1(ms: &[Machine]) -> u16 {
    ms.iter().map(Machine::part_1).sum()
}

#[allow(clippy::cast_possible_truncation)]
#[allow(clippy::cast_sign_loss)]
impl Machine {
    // With help from https://www.reddit.com/r/adventofcode/comments/1pity70/comment/nta6jn9
    fn part_2(&self) -> u16 {
        variables!(problem: 0 <= presses[self.wiring.len()] (integer));
        let mut counts = vec![Expression::with_capacity(self.wiring.len()); self.joltage.len()];
        for (wires, &p) in self.wiring.iter().zip(presses.iter()) {
            for &w in wires {
                counts[w as usize] += p;
            }
        }
        let mut model = problem
            .minimise(presses.iter().sum::<Expression>())
            .using(highs);
        for (c, &j) in counts.into_iter().zip(self.joltage.iter()) {
            model.add_constraint(constraint!(c == j));
        }
        let solution = model.solve().expect("part_2");
        presses.iter().map(|&p| solution.value(p)).sum::<f64>() as u16
    }
}

fn part_2(ms: &[Machine]) -> u16 {
    ms.iter().map(Machine::part_2).sum()
}

fn main() {
    let input = parse::it(include_str!("../input.txt")).expect("parse");
    println!("{}", part_1(&input));
    println!("{}", part_2(&input));
}
