fn part_1(s: &str) -> usize {
    let mut splits = 0;
    let start = s.trim().lines().next().expect("start");
    let mut beams = vec![false; start.len()];
    beams[start.find('S').expect("S")] = true;
    for line in s.trim().lines().skip(1) {
        for (i, c) in line.char_indices() {
            if c == '^' && beams[i] {
                splits += 1;
                beams[i] = false;
                beams[i - 1] = true;
                beams[i + 1] = true;
            }
        }
    }
    splits
}

fn main() {
    let input = std::fs::read_to_string("input.txt").expect("file");
    println!("{}", part_1(&input));
}
