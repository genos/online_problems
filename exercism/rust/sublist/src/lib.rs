#![feature(slice_patterns)]

#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(xs: &[T], ys: &[T]) -> Comparison {
    if xs.len() >= ys.len() {
        cmp_larger_short(xs, ys)
    } else {
        match cmp_larger_short(ys, xs) {
            Comparison::Equal => Comparison::Equal,
            Comparison::Sublist => Comparison::Superlist,
            Comparison::Superlist => Comparison::Sublist,
            Comparison::Unequal => Comparison::Unequal,
        }
    }
}

// This works but is inefficient; I'd prefer a more intelligent search.
fn cmp_larger_short<T: PartialEq>(larger: &[T], shorter: &[T]) -> Comparison {
    if larger.is_empty() {
        Comparison::Equal
    } else if shorter.is_empty() {
        Comparison::Superlist
    } else {
        for slice in larger.windows(shorter.len()) {
            if slice == shorter {
                if slice.len() == larger.len() {
                    return Comparison::Equal;
                } else {
                    return Comparison::Superlist;
                }
            }
        }
        Comparison::Unequal
    }
}
