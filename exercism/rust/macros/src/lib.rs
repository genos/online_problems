#[macro_export]
macro_rules! hashmap {
    // typical usage; handles empty, no trailing comma, etc.
    ($($key:expr => $value:expr),*) => {
        {
            let mut h = ::std::collections::HashMap::new();
            $(h.insert($key, $value);)*
            h
        }
    };
    // needed to handle trailing comma
    ($($key:expr => $value:expr,)*) => { hashmap!( $($key => $value),* ) };
}
