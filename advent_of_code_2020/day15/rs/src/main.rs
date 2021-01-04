// copy-paste
static INPUT: [usize; 6] = [1, 0, 18, 10, 19, 6];

fn run(n: usize) -> usize {
    // setup
    let mut i = 0;
    let mut x = 0;
    let mut v = vec![0; n];
    for &input in INPUT.iter() {
        v[x] = i;
        x = input;
        i += 1;
    }
    // loop
    while i < n {
        let last_time = v[x];
        v[x] = i;
        x = if last_time == 0 { 0 } else { i - last_time };
        i += 1;
    }
    // fin
    x
}

fn main() {
    dbg!(run(2020));
    dbg!(run(30_000_000));
}
