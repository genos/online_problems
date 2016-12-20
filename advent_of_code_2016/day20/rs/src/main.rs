extern crate regex;
use regex::Regex;

const MIN_BOUND: u32 = 0;
const MAX_BOUND: u32 = 10;//((1u64 << 32) - 1) as u32;

#[derive(Debug)]
struct Range {
    lo: u32,
    hi: u32,
}

impl Range {
    fn default() -> Range {
        Range {
            lo: MIN_BOUND,
            hi: MAX_BOUND,
        }
    }
    fn new(lo: u32, hi: u32) -> Range {
        assert!(lo <= hi);
        Range { lo: lo, hi: hi }
    }
    fn parse(s: &str) -> Range {
        let re: Regex = Regex::new(r"^(\d+)-(\d+)$").unwrap();
        match re.captures(s) {
            None => Range::default(),
            Some(caps) => {
                match (caps.at(1), caps.at(2)) {
                    (Some(a), Some(b)) => {
                        Range::new(a.parse().unwrap_or(MIN_BOUND),
                                   b.parse().unwrap_or(MAX_BOUND))
                    }
                    _ => Range::default(),
                }
            }
        }
    }
}

impl Iterator for Range {
    type Item = u32;
    fn next(&mut self) -> Option<u32> {
        let lo = self.lo;
        let hi = self.hi;
        self.lo += 1;
        if lo > hi {
            None
        } else {
            Some(lo)
        }
    }
}

fn mark_off(a: &mut[bool; MAX_BOUND as usize], r: Range) {
    for i in r {
        a[i as usize] = false;
    }
}

fn main() {
    let test_input = "5-8\n0-2\n4-7";
    let ranges = test_input.lines().map(Range::parse);
    let mut a = [true; MAX_BOUND as usize];
    for r in ranges {
        println!("{:?}", r);
        mark_off(&mut a, r);
    }
    for i in MIN_BOUND .. MAX_BOUND {
        if a[i as usize] {
            println!("{}", i);
            break;
        }
    }
}
