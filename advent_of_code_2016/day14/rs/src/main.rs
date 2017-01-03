extern crate crypto;
use crypto::md5::Md5;
use crypto::digest::Digest;

fn hash(salt: &str, n: u32, loops: usize) -> String {
    let mut md5 = Md5::new();
    md5.input_str(salt);
    md5.input_str(&n.to_string());
    let mut res = md5.result_str();
    for _ in 0..loops {
        md5.reset();
        md5.input_str(&res);
        res = md5.result_str();
    }
    res
}

fn first_triple(s: String) -> Option<char> {
    s.chars()
        .collect::<Vec<_>>()
        .windows(3)
        .find(|x| x[0] == x[1] && x[1] == x[2])
        .map(|x| x[0])
}

#[test]
fn _1st_triple() {
    let salt = "abc";
    for i in 0..18 {
        assert!(first_triple(hash(salt, i, 0)).is_none());
    }
    assert!(first_triple(hash(salt, 18, 0)) == Some('8'));
}

fn has_five(s: String, c: char) -> bool {
    s.chars()
        .collect::<Vec<_>>()
        .windows(5)
        .any(|x| x.iter().all(|&y| y == c))
}

#[test]
fn _has_5() {
    let salt = "abc";
    for i in 19..39 {
        assert!(first_triple(hash(salt, i, 0)).is_none());
    }
    assert!(first_triple(hash(salt, 39, 0)) == Some('e'));
    for i in 1..816 {
        assert!(!has_five(hash(salt, i, 0), 'e'));
    }
    assert!(has_five(hash(salt, 816, 0), 'e'));
}

fn is_key(salt: &str, n: u32, loops: usize) -> bool {
    if let Some(c) = first_triple(hash(salt, n, loops)) {
        for i in 1..1000 {
            if has_five(hash(salt, n + i, loops), c) {
                return true;
            }
        }
        false
    } else {
        false
    }
}

#[test]
fn _is_key() {
    let salt = "abc";
    for i in 0..39 {
        assert!(!is_key(salt, i, 0));
    }
    assert!(is_key(salt, 39, 0));
    for i in 40..92 {
        assert!(!is_key(salt, i, 0));
    }
    assert!(is_key(salt, 92, 0));
}

fn sixty_fourth_key(salt: &str, loops: usize, verbose: bool) -> u32 {
    let mut n = 0;
    let mut i = 0;
    while n < 64 {
        if is_key(salt, i, loops) {
            if verbose {
                println!("{}: {}", n, i);
            }
            n += 1;
        }
        i += 1;
    }
    i - 1
}

#[test]
fn _64th_key() {
    assert!(sixty_fourth_key("abc", 0, false) == 22728);
}

#[test]
fn _pt2() {
    let salt = "abc";
    let loops = 2016;
    for i in 0..5 {
        assert!(first_triple(hash(salt, i, loops)).is_none());
    }
    assert!(first_triple(hash(salt, 5, loops)) == Some('2'));
    assert!(!is_key(salt, 5, loops));
    for i in 6..10 {
        assert!(first_triple(hash(salt, i, loops)).is_none());
    }
    assert!(first_triple(hash(salt, 10, loops)) == Some('e'));
    assert!(is_key(salt, 10, loops));
    // assert!(sixty_fourth_key("abc", 2016, true) == 22551); // slow
}

fn main() {
    let salt = "yjdafjpo";
    println!("{}", sixty_fourth_key(salt, 0, false));
    println!("{}", sixty_fourth_key(salt, 2016, true));
}
