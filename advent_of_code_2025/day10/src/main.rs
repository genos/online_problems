use pathfinding::prelude::dijkstra;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Machine {
    lights: Vec<bool>,
    wiring: Vec<Vec<u16>>,
    joltage: Vec<u16>,
}

impl Machine {
    fn press(&self, button: usize) -> Self {
        let mut m = self.clone();
        for w in &self.wiring[button] {
            m.lights[*w as usize] = !m.lights[*w as usize]
        }
        m
    }
    fn part_1(&self) -> usize {
        let result = dijkstra(
            self,
            |m| {
                (0..m.wiring.len())
                    .map(|b| (m.press(b), 1))
                    .collect::<Vec<_>>()
            },
            |m| !m.lights.iter().any(|&l| l),
        );
        result.expect("astar").1
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

fn part_1(ms: &[Machine]) -> usize {
    ms.iter().map(|m| m.part_1()).sum()
}

fn main() {
    let input = parse(&std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", part_1(&input));
}
