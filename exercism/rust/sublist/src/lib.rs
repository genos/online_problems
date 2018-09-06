#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(xs: &[T], ys: &[T]) -> Comparison {
    if xs == ys {
        Comparison::Equal
    } else if xs.len() < ys.len() && is_sublist(xs, ys) {
        Comparison::Sublist
    } else if is_sublist(ys, xs) {
        Comparison::Superlist
    } else {
        Comparison::Unequal
    }
}

// This works but is inefficient; I'd prefer a more intelligent search.
fn is_sublist<T: PartialEq>(short: &[T], long: &[T]) -> bool {
    short.is_empty() || long.windows(short.len()).any(|slice| slice == short)
}
