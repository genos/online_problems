pub fn raindrops(n: u64) -> String {
    let s = format!("{}{}{}",
                    f(n, 3, "Pling"),
                    f(n, 5, "Plang"),
                    f(n, 7, "Plong"));
    if s.is_empty() { n.to_string() } else { s }
}

fn f(n: u64, m: u64, s: &str) -> &str {
    if n % m == 0 { s } else { "" }
}
