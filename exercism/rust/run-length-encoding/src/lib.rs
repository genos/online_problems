extern crate itertools;
#[macro_use]
extern crate lazy_static;
extern crate regex;

use itertools::Itertools;
use regex::Regex;

pub fn encode(s: &str) -> String {
    fn f(c: char, n: usize) -> String {
        if n == 1 {
            format!("{}", c)
        } else {
            format!("{}{}", n, c)
        }
    }
    s.chars().group_by(|&c| c).into_iter().map(|(c, cs)| f(c, cs.count())).collect()
}

pub fn decode(s: &str) -> String {
    lazy_static! {
        static ref R: Regex = Regex::new(r"(\d*)(\D)").unwrap();
    }
    R.captures_iter(s).map(|c| c[2].repeat(c[1].parse().unwrap_or(1))).collect()
}
