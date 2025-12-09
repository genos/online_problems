fn solve(s: &str) -> (u64, u64) {
    let (first, rest) = s.trim().split_once('\n').expect("newline");
    let (mut splits, mut beams) = (0, vec![0; first.len()]);
    beams[first.find('S').expect("S")] = 1;
    for line in rest.lines() {
        for (i, c) in line.char_indices() {
            if c == '^' && beams[i] > 0 {
                splits += 1;
                beams[i - 1] += beams[i];
                beams[i + 1] += beams[i];
                beams[i] = 0;
            }
        }
    }
    (splits, beams.iter().sum())
}

fn main() {
    let (part_1, part_2) = solve(&std::fs::read_to_string("input.txt").expect("file"));
    println!("{part_1}\n{part_2}");
}
