#[derive(Debug)]
struct Worksheet {
    nums: Vec<Vec<u64>>,
    ops: Vec<char>,
}

impl From<String> for Worksheet {
    fn from(s: String) -> Self {
        let (mut nums_t, mut ops) = (Vec::<Vec<u64>>::new(), Vec::new());
        s.trim().lines().for_each(|line| {
            if line.contains('*') || line.contains('+') {
                ops.extend(line.chars().filter(|&c| c == '*' || c == '+'));
            } else {
                nums_t.push(
                    line.split_whitespace()
                        .map(|s| s.parse().expect("num"))
                        .collect::<Vec<u64>>(),
                );
            }
        });
        let mut nums = vec![vec![0; nums_t.len()]; nums_t[0].len()];
        for i in 0..nums_t.len() {
            for (j, &n) in nums_t[i].iter().enumerate() {
                nums[j][i] = n;
            }
        }
        Self { nums, ops }
    }
}

impl Worksheet {
    fn part_1(&self) -> u64 {
        self.nums
            .iter()
            .enumerate()
            .map(|(i, col)| match self.ops[i] {
                '*' => col.iter().product::<u64>(),
                '+' => col.iter().sum(),
                _ => unreachable!("By design"),
            })
            .sum()
    }
}

fn main() {
    let input = Worksheet::from(std::fs::read_to_string("test.txt").expect("file"));
    println!("{}", input.part_1());
}
