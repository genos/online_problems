pub struct Brackets {
    ok: bool,
    seen: Vec<char>,
}

impl<'a> From<&'a str> for Brackets {
    fn from(input: &str) -> Self {
        let mut b = Brackets {
            ok: true,
            seen: Vec::new(),
        };
        for c in input.chars() {
            match c {
                '[' | '{' | '(' => b.seen.push(c),
                ']' | '}' | ')' => {
                    b.ok = b.seen.pop().map_or(false, |x| match c {
                        ']' => x == '[',
                        '}' => x == '{',
                        ')' => x == '(',
                        _ => panic!("Not a bracket pair character"),
                    });
                }
                _ => (),
            }
            if !b.ok {
                break;
            }
        }
        b.ok &= b.seen.is_empty();
        b
    }
}

impl Brackets {
    pub fn are_balanced(&self) -> bool {
        self.ok
    }
}
