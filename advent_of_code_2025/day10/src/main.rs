use good_lp::{Expression, ProblemVariables, Solution, SolverModel, constraint, highs, variable};
use std::collections::{HashSet, VecDeque};

struct Machine {
    lights: Vec<bool>,
    wiring: Vec<Vec<u16>>,
    joltage: Vec<u16>,
}

peg::parser! {
    grammar machine_parser() for str {
        pub rule machines() -> Vec<Machine> = machine() ** "\n"
        rule machine() -> Machine = lights:l() _ wiring:ws() _ joltage:j() { Machine { lights, wiring, joltage } }
        rule _() = quiet!{[' '|'\t']*}
        rule l() -> Vec<bool> = "[" l:$(['.'|'#']*) "]" { l.chars().map(|c| c == '#').collect() }
        rule ws() -> Vec<Vec<u16>> = w() ** " "
        rule j() -> Vec<u16> = "{" ns:(n() ** ",") "}" { ns }
        rule w() -> Vec<u16> = "(" ns:(n() ** ",") ")" { ns }
        rule n() -> u16 = n:$(['0'..='9']+) { n.parse().expect("num") }
    }
}

fn parse(s: &str) -> Vec<Machine> {
    machine_parser::machines(s).expect("parse")
}

impl Machine {
    fn part_1(&self) -> u64 {
        let mut seen = HashSet::new();
        let mut todo = VecDeque::from([(
            self.lights
                .iter()
                .enumerate()
                .map(|(i, &b)| u16::from(b) * (1 << i))
                .fold(0, |x, y| x | y),
            0,
        )]);
        while let Some((lights, steps)) = todo.pop_front() {
            if seen.contains(&lights) {
                continue;
            }
            if lights == 0 {
                return steps;
            }
            seen.insert(lights);
            for wires in &self.wiring {
                todo.push_back((wires.iter().fold(lights, |ls, w| ls ^ (1 << w)), steps + 1));
            }
        }
        unreachable!("part_1")
    }
}

fn part_1(ms: &[Machine]) -> u64 {
    ms.iter().map(Machine::part_1).sum()
}

#[allow(clippy::cast_possible_truncation)]
#[allow(clippy::cast_sign_loss)]
impl Machine {
    // With help from https://www.reddit.com/r/adventofcode/comments/1pity70/comment/nta6jn9
    fn part_2(&self) -> u64 {
        let mut vars = ProblemVariables::new();
        let presses = vars.add_vector(variable().integer().min(0), self.wiring.len());
        let mut counts = vec![Expression::with_capacity(self.wiring.len()); self.joltage.len()];
        for (i, wires) in self.wiring.iter().enumerate() {
            for &w in wires {
                counts[w as usize] += presses[i];
            }
        }
        let mut model = vars
            .minimise(presses.iter().sum::<Expression>())
            .using(highs);
        for (c, &j) in counts.into_iter().zip(self.joltage.iter()) {
            model.add_constraint(constraint!(c == j));
        }
        let solution = model.solve().expect("part_2");
        presses.iter().map(|&p| solution.value(p)).sum::<f64>() as u64
    }
}

fn part_2(ms: &[Machine]) -> u64 {
    ms.iter().map(Machine::part_2).sum()
}

fn main() {
    let input = parse(&std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", part_1(&input));
    println!("{}", part_2(&input));
}
