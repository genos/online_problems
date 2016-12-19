extern crate itertools;
use itertools::Itertools;

fn str_to_bv(s: &str) -> Vec<bool> {
    s.chars().map(|c| c == '1').collect()
}

fn bv_to_str(b: Vec<bool>) -> String {
    b.iter().map(|x| if *x { '1' } else { '0' }).collect()
}

fn dragon(b: &mut Vec<bool>) {
    let n = b.len();
    b.resize(2 * n + 1, false);
    for i in 0..n {
        b[n + 1 + i] = !b[n - i - 1];
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
        dragon(&mut x);
        assert_eq!(bv_to_str(x), b);
    }
}

fn check_step(b: &mut Vec<bool>) {
    let n = b.len();
    let c = b.iter().tuples().map(|(x, y)| x == y).collect::<Vec<_>>();
    b.truncate(n / 2);
    for i in 0..b.len() {
        b[i] = c[i];
    }
}

#[test]
fn check_step_test() {
    for (a, b) in vec![("110010110100", "110101"),
                       ("110101", "100"),
                       ("10000011110010000111", "0111110101"),
                       ("0111110101", "01100")] {
        let mut x = str_to_bv(a);
        check_step(&mut x);
        assert_eq!(bv_to_str(x), b);
    }
}

fn check_sum(b: &mut Vec<bool>, l: usize) {
    while b.len() < l {
        dragon(b);
    }
    b.truncate(l);
    while b.len() & 1 == 0 {
        check_step(b);
    }
}

#[test]
fn check_sum_test() {
    let mut a = str_to_bv("110010110100");
    check_sum(&mut a, 12);
    assert_eq!(bv_to_str(a), "100");
    let mut b = str_to_bv("10000");
    check_sum(&mut b, 20);
    assert_eq!(bv_to_str(b), "01100");
}

fn main() {
    let mut input1 = str_to_bv("10001001100000001");
    check_sum(&mut input1, 272);
    println!("{}", bv_to_str(input1));
    let mut input2 = str_to_bv("10001001100000001");
    check_sum(&mut input2, 35651584);
    println!("{}", bv_to_str(input2));
}
