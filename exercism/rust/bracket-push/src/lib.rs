pub struct Brackets {
    ok: bool,
    brack: i64,
    brace: i64,
    paren: i64,
    seen: Vec<char>,
}

impl Default for Brackets {
    fn default() -> Self {
        Brackets {
            ok: true,
            brack: 0,
            brace: 0,
            paren: 0,
            seen: Vec::new(),
        }
    }
}

impl<'a> From<&'a str> for Brackets {
    fn from(input: &str) -> Self {
        let mut b = Brackets::default();
        for c in input.chars() {
            match c {
                '[' => {
                    b.brack += 1;
                    b.seen.push('[');
                }
                ']' => {
                    b.brack -= 1;
                    b.ok = b.seen.pop().map_or(false, |x| x == '[');
                }
                '{' => {
                    b.brace += 1;
                    b.seen.push('{');
                }
                '}' => {
                    b.brace -= 1;
                    b.ok = b.seen.pop().map_or(false, |x| x == '{');
                }
                '(' => {
                    b.paren += 1;
                    b.seen.push('(');
                }
                ')' => {
                    b.paren -= 1;
                    b.ok = b.seen.pop().map_or(false, |x| x == '(');
                }
                _ => (),
            }
        }
        b
    }
}

impl Brackets {
    pub fn are_balanced(&self) -> bool {
        self.ok && self.brack == 0 && self.brace == 0 && self.paren == 0
    }
}
