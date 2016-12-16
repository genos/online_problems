extern crate bit_vec;
extern crate itertools;
use bit_vec::BitVec;
use itertools::Itertools;

fn str_to_bv(s: &str) -> BitVec {
    let mut b = BitVec::from_elem(s.len(), false);
    for (i, c) in s.char_indices() {
        b.set(i, c == '1');
    }
    b
}

fn dragon(b: &mut BitVec) {
    let n = b.len();
    let mut c: BitVec = b.iter().rev().collect();
    c.negate();
    b.grow(n + 1, false);
    for i in 0..n {
        b.set(n + 1 + i, c.get(i).unwrap());
    }
}

#[test]
fn dragon_test() {
    for (a, b) in vec![("1", "100"),
                       ("0", "001"),
                       ("10000", "10000011110"),
                       ("10000011110", "10000011110010000111110"),
                       ("11111", "11111000000"),
                       ("111100001010", "1111000010100101011110000")] {
        let mut x = str_to_bv(a);
        let y = str_to_bv(b);
        dragon(&mut x);
        assert_eq!(x, y);
    }
}

fn check_step(b: &mut BitVec) {
    let n = b.len();
    let c = b.iter().tuples().map(|(x, y)| x == y).collect::<Vec<_>>();
    b.truncate(n / 2);
    for i in 0..n / 2 {
        b.set(i, c[i]);
    }
}

#[test]
fn check_step_test() {
    for (a, b) in vec![("110010110100", "110101"),
                       ("110101", "100"),
                       ("10000011110010000111", "0111110101"),
                       ("0111110101", "01100")] {
        let mut x = str_to_bv(a);
        let y = str_to_bv(b);
        check_step(&mut x);
        assert_eq!(x, y);
    }
}

fn check_sum(b: &mut BitVec, l: usize) {
    // expand
    while b.len() < l {
        dragon(b);
    }
    // cut
    b.truncate(l);
    // shrink
    while b.len() & 1 == 0 {
        check_step(b);
    }
}

#[test]
fn check_sum_test() {
    let mut a = str_to_bv("110010110100");
    check_sum(&mut a, 12);
    assert_eq!(a, str_to_bv("100"));
    let mut b = str_to_bv("10000");
    check_sum(&mut b, 20);
    assert_eq!(b, str_to_bv("01100"));
}

fn main() {
    let mut input1 = str_to_bv("10001001100000001");
    check_sum(&mut input1, 272);
    println!("{:?}", input1);
    let mut input2 = str_to_bv("10001001100000001");
    check_sum(&mut input2, 35651584);
    println!("{:?}", input2);
}
