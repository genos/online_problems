pub struct Brackets {
    ok: bool,
    seen: Vec<char>,
}

impl Default for Brackets {
    fn default() -> Self {
        Brackets {
            ok: true,
            seen: Vec::new(),
        }
    }
}

impl<'a> From<&'a str> for Brackets {
    fn from(input: &str) -> Self {
        let mut b = Brackets::default();
        for c in input.chars() {
            match c {
                '[' | '{' | '(' => b.seen.push(c),
                ']' | '}' | ')' => b.pop_and_pair(c),
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
    fn pop_and_pair(&mut self, c: char) {
        self.ok = self.seen.pop().map_or(false, |x| match c {
            ']' => x == '[',
            '}' => x == '{',
            ')' => x == '(',
            _ => panic!("Not a bracket pair character"),
        });
    }
}
