extern crate crypto;
use crypto::md5::Md5;
use crypto::digest::Digest;

const KEY: &'static str = "ckczppom";

fn five_zeros(m: &mut Md5, input: &[u8]) -> bool {
    m.reset();
    m.input(input);
    let mut output = [0; 16];
    m.result(&mut output);
    output[..2] == [0, 0] && output[2] <= 0x0F
}

fn to_input(key: &[u8], i: u64) -> Vec<u8> {
    [key, i.to_string().as_bytes()].concat()
}

#[test]
fn _t0() {
    let mut m = Md5::new();
    let key = "abcdef".as_bytes();
    for i in 1..609043 {
        assert!(!five_zeros(&mut m, &to_input(key, i)));
    }
    assert!(five_zeros(&mut m, &to_input(key, 609043)));
}

#[test]
fn _t1() {
    let mut m = Md5::new();
    let key = "pqrstuv".as_bytes();
    for i in 1..1048970 {
        assert!(!five_zeros(&mut m, &to_input(key, i)));
    }
    assert!(five_zeros(&mut m, &to_input(key, 1048970)));
}

fn answer1() -> Option<u64> {
    let mut m = Md5::new();
    for i in 1..std::u64::MAX {
        if five_zeros(&mut m, &to_input(KEY.as_bytes(), i)) {
            return Some(i);
        }
    }
    None
}

fn six_zeros(m: &mut Md5, input: &[u8]) -> bool {
    m.reset();
    m.input(input);
    let mut output = [0; 16];
    m.result(&mut output);
    output[..3] == [0, 0, 0]
}

fn answer2() -> Option<u64> {
    let mut m = Md5::new();
    for i in 1..std::u64::MAX {
        if six_zeros(&mut m, &to_input(KEY.as_bytes(), i)) {
            return Some(i);
        }
    }
    None
}

fn main() {
    match answer1() {
        Some(i) => println!("{}", i),
        None => println!("Error in answer1"),
    }
    match answer2() {
        Some(i) => println!("{}", i),
        None => println!("Error in answer2"),
    }
}
