use pathfinding::prelude::dijkstra;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Machine {
    lights: Vec<bool>,
    wiring: Vec<Vec<u16>>,
    joltage: Vec<u16>,
}

fn press_1(lights: &[bool], wiring: &[Vec<u16>]) -> Vec<(Vec<bool>, u16)> {
    wiring
        .iter()
        .map(|wires| {
            let mut ls = lights.to_vec();
            for w in wires {
                ls[*w as usize] = !ls[*w as usize];
            }
            (ls, 1)
        })
        .collect()
}

impl Machine {
    fn part_1(&self) -> u16 {
        dijkstra(
            &self.lights,
            |ls| press_1(ls, &self.wiring),
            |ls| !ls.iter().any(|&l| l),
        )
        .expect("part_1")
        .1
    }
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

fn part_1(ms: &[Machine]) -> u16 {
    ms.iter().map(Machine::part_1).sum()
}

fn main() {
    let input = parse(&std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", part_1(&input));
}
