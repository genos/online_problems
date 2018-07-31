pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<(usize, usize)> {
    let mut result: Vec<(usize, usize)> = Vec::new();
    for i in 0..input.len() {
        for j in 0..input[i].len() {
            if is_saddle_point(input, i, j) {
                result.push((i, j));
            }
        }
    }
    result
}

fn is_saddle_point(input: &[Vec<u64>], i: usize, j: usize) -> bool {
    let x = input.get(i).and_then(|v| v.get(j));
    (x == input[i].iter().max()) && (x == input.iter().map(|v| &v[j]).min())
}
