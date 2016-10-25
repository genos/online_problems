fn a(n: i64) -> String {
    match n {
        0 => "No more".to_string(),
        -1 => 99.to_string(),
        _ => n.to_string(),
    }
}

fn b(n: i64) -> String {
    (if n == 1 { "bottle" } else { "bottles" }).to_string()
}

fn c(n: i64) -> String {
    if n == 0 {
        "Go to the store and buy some more".to_string()
    } else {
        format!("Take {0} down and pass it around",
                if n == 1 { "it" } else { "one" })
    }
}

pub fn verse(n: i64) -> String {
    format!("{0} {1} of beer on the wall, {2} {1} of beer.\n{3}, {4} {5} of beer on the wall.\n",
            a(n),
            b(n),
            a(n).to_lowercase(),
            c(n),
            a(n - 1).to_lowercase(),
            b(n - 1))
}

pub fn sing(high: i64, low: i64) -> String {
    (low..high + 1).rev().map(verse).collect::<Vec<_>>().join("\n")
}
