use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
#[macro_use]
extern crate lazy_static;
extern crate regex;
use regex::Regex;

fn read_input() -> String {
    let mut f = File::open("../input.txt").expect("'input.txt' not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("unable to read 'input.txt'");
    contents
}

#[derive(Debug, PartialEq)]
enum Command {
    Inc,
    Dec,
}

impl Command {
    fn parse(s: &str) -> Command {
        match s {
            "inc" => Command::Inc,
            "dec" => Command::Dec,
            _ => panic!("unable to parse command"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Relation {
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
}

impl Relation {
    fn parse(s: &str) -> Relation {
        match s {
            "==" => Relation::Equal,
            "!=" => Relation::NotEqual,
            ">" => Relation::Greater,
            "<" => Relation::Less,
            ">=" => Relation::GreaterEqual,
            "<=" => Relation::LessEqual,
            _ => panic!("unable to parse relation"),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Instruction {
    register: String,
    command: Command,
    amount: i64,
    referrent: String,
    relation: Relation,
    value: i64,
}

fn event(h: &HashMap<String, i64>, i: &Instruction) -> bool {
    let referrent = *h.get(&i.referrent).unwrap_or(&0);
    match i.relation {
        Relation::Equal => referrent == i.value,
        Relation::NotEqual => referrent != i.value,
        Relation::Greater => referrent > i.value,
        Relation::Less => referrent < i.value,
        Relation::GreaterEqual => referrent >= i.value,
        Relation::LessEqual => referrent <= i.value,
    }
}

fn line_to_instruction(line: &str) -> Instruction {
    lazy_static! {
        static ref R: Regex =
            Regex::new(r"(?x)
                       \A
                       (?P<register>\w+)\s
                       (?P<command>(inc|dec))\s
                       (?P<amount>([-]?)\d+)\s
                       if\s
                       (?P<referrent>\w+)\s
                       (?P<relation>(==|!=|>|<|>=|<=))\s
                       (?P<value>([-]?)\d+)
                       \z
                       ").unwrap();
    }
    let c = R.captures(line).expect("invalid line input");
    Instruction {
        register: c["register"].to_owned(),
        command: Command::parse(&c["command"]),
        amount: c["amount"].parse().expect("unable to parse amount"),
        referrent: c["referrent"].to_owned(),
        relation: Relation::parse(&c["relation"]),
        value: c["value"].parse().expect("unable to parse value"),
    }
}

fn parse_input(input: &str) -> Vec<Instruction> {
    input.lines().map(line_to_instruction).collect()
}

fn exec_instruction_yielding_max(h: &mut HashMap<String, i64>, i: &Instruction, m: i64) -> i64 {
    if event(h, i) {
        let r = h.entry(i.register.clone()).or_insert(0);
        match i.command {
            Command::Inc => *r += i.amount,
            Command::Dec => *r -= i.amount,
        }
    }
    std::cmp::max(*h.values().max().expect("no max!?"), m)
}

fn part1(input: &[Instruction]) -> i64 {
    let mut h = HashMap::new();
    for i in input {
        exec_instruction_yielding_max(&mut h, i, 0);
    }
    *h.values().max().expect("no max!?")
}

fn part2(input: &[Instruction]) -> i64 {
    let mut h = HashMap::new();
    let mut m = -1;
    for i in input {
        m = exec_instruction_yielding_max(&mut h, i, m);
    }
    m
}

fn main() {
    let input = parse_input(&read_input());
    println!("{}", part1(&input));
    println!("{}", part2(&input));
}
