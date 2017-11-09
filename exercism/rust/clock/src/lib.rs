use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub struct Clock {
    minutes: i64,
}

impl Clock {
    pub fn new(hours: i64, minutes: i64) -> Clock {
        Clock {
            minutes: to_minutes((60 * hours) + minutes),
        }
    }

    pub fn add_minutes(&self, minutes: i64) -> Clock {
        Clock {
            minutes: to_minutes(self.minutes + minutes),
        }
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:02}:{:02}", self.minutes / 60, self.minutes % 60)
    }
}

fn to_minutes(c: i64) -> i64 {
    c.modulo(1440)
}

// % is `rem`, _not_ `modulo`!
// https://stackoverflow.com/a/41422009
pub trait ModuloSignedExt {
    fn modulo(&self, n: Self) -> Self;
}
macro_rules! modulo_signed_ext_impl {
    ($($t:ty)*) => ($(
        impl ModuloSignedExt for $t {
            #[inline]
            fn modulo(&self, n: Self) -> Self {
                (self % n + n) % n
            }
        }
    )*)
}
modulo_signed_ext_impl! { i8 i16 i32 i64 }
