#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

impl Comparison {
    fn dual(self) -> Comparison {
        match self {
            Comparison::Sublist => Comparison::Superlist,
            Comparison::Superlist => Comparison::Sublist,
            c => c,
        }
    }
}

pub fn sublist<T: PartialEq>(xs: &[T], ys: &[T]) -> Comparison {
    if xs.len() >= ys.len() {
        cmp_larger_shorter(xs, ys)
    } else {
        cmp_larger_shorter(ys, xs).dual()
    }
}

// This also works but is inefficient; I'd prefer a more intelligent search.
fn cmp_larger_shorter<T: PartialEq>(larger: &[T], shorter: &[T]) -> Comparison {
    if larger.is_empty() {
        Comparison::Equal
    } else if shorter.is_empty() {
        Comparison::Superlist
    } else if larger.windows(shorter.len()).any(|slice| slice == shorter) {
        if larger.len() == shorter.len() {
            Comparison::Equal
        } else {
            Comparison::Superlist
        }
    } else {
        Comparison::Unequal
    }
}
