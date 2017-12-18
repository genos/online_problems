use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
#[macro_use]
extern crate lazy_static;
extern crate regex;
use regex::Regex;

#[derive(Debug, PartialEq)]
struct Program {
    name: String,
    weight: u64,
    above: Vec<String>,
}

fn read_input() -> String {
    let mut f = File::open("input.txt").expect("'input.txt' not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("unable to read 'input.txt'");
    contents
}

fn line_to_program(line: &str) -> Program {
    lazy_static! {
        static ref R: Regex = Regex::new(r"(?x)
                                         \A                     # start of line
                                         (?P<name>[a-zA-Z]+)\s  # name: string
                                         \((?P<weight>[0-9]+)\) # weight: u64
                                         (\s->\s(?P<above>.*))? # -> comma sep.
                                         \z                     # end of line"
                                         ).unwrap();
    }
    let cs = R.captures(line).expect("Invalid line input");
    let n = cs["name"].to_owned();
    let w = cs["weight"].parse().expect("Unable to parse number");
    let mut a = Vec::new();
    for other in cs.name("above")
        .map(|m| m.as_str())
        .unwrap_or("")
        .split(", ")
    {
        if !other.is_empty() {
            a.push(other.to_owned());
        }
    }
    Program {
        name: n,
        weight: w,
        above: a,
    }
}

fn parse_input(input: &str) -> Vec<Program> {
    input.lines().map(line_to_program).collect()
}

fn part1(input: &[Program]) -> String {
    let mut h = HashMap::new();
    for p in input {
        for q in &p.above {
            h.insert(q, &p.name);
        }
    }
    let sentinel = &"SENTINEL".to_string();
    let mut s = *h.keys().next().unwrap_or(&sentinel);
    while h.contains_key(s) {
        s = h[s];
    }
    s.to_owned()
}

fn find_unbalanced(name: &str, map: &HashMap<String, &Program>) -> Option<String> {
    let p = map[name];
    let ps: Vec<&Program> = p.above.iter().map(|name| map[name]).collect();
    if ps.is_empty() {
        None
    } else {
        let mut w: HashMap<u64, Vec<String>> = HashMap::new();
        for p in ps {
            let x = w.entry(p.weight).or_insert_with(Vec::new);
            x.push(p.name.clone());
        }
        match w.len() {
            0 => panic!("Impossible: empty <above>"),
            1 => None,
            2 => {
                let lesser: &Vec<String> = w.values().min_by_key(|&names| names.len()).unwrap();
                lesser.iter().cloned().next()
            },
            _ => panic!("More than one program with differing weights"),
        }
    }
}

fn compile_map(input: &[Program]) -> HashMap<String, &Program> {
    let mut h = HashMap::new();
    for p in input {
        h.insert(p.name.clone(), p);
    }
    h
}

fn part2(bottom: &str, input: &[Program]) -> u64 {
    let map = compile_map(input);
    let mut name = Some(bottom.to_string());
    let mut trailer = "".to_string();
    while name.is_some() {
        let n = name.unwrap();
        match find_unbalanced(&n, &map) {
            Some(s) => {
                trailer = n;
                name = Some(s);
            },
            _ => break,
        }
    }
    let ws = map[&trailer].above.iter().map(|p| map[p].weight).collect::<Vec<_>>();
    match (ws.iter().max(), ws.iter().min()) {
        (Some(a), Some(b)) => a - b,
        _ => panic!("Pandas"),
    }
}

fn main() {
    let input = parse_input(&read_input());
    let bottom = part1(&input);
    println!("{}", bottom);
    println!("{:?}", input.iter().find(|p| p.name == bottom));
    println!("{}", part2(&bottom, &input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_to_program() {
        assert_eq!(
            line_to_program("asdf (1729)"),
            Program {
                name: "asdf".to_string(),
                weight: 1729,
                above: Vec::new(),
            }
        );
        assert_eq!(
            line_to_program("asdf (1729) -> abc"),
            Program {
                name: "asdf".to_string(),
                weight: 1729,
                above: vec!["abc".to_string()],
            }
        );
        assert_eq!(
            line_to_program("asdf (1729) -> abc, xyz"),
            Program {
                name: "asdf".to_string(),
                weight: 1729,
                above: vec!["abc".to_string(), "xyz".to_string()],
            }
        );
    }

}
