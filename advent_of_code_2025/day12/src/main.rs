// cheated
fn main() {
    println!(
        "{}",
        &std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/input.txt"))
            .expect("file")
            .trim()
            .lines()
            .filter(|line| {
                line.contains('x') && {
                    let (first, rest) = line.split_once(": ").expect("format");
                    let (m, n) = first.split_once('x').expect("x");
                    let counts = rest
                        .split_ascii_whitespace()
                        .map(|c| c.parse::<u16>().expect("c"));
                    let (w, h) = (m.parse::<u16>().expect("m"), n.parse::<u16>().expect("n"));
                    w * h >= 9 * counts.sum::<u16>()
                }
            })
            .count()
    );
}
