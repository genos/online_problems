enum Op {
    Mul,
    Add,
}

struct Worksheet {
    ops: Vec<Op>,
    nums: Vec<Vec<Vec<Option<u64>>>>,
}

impl From<String> for Worksheet {
    fn from(s: String) -> Self {
        let mut lines = s.lines();
        let ops_line = lines.next_back().expect("ops line");
        let (mut ix, ops): (Vec<usize>, Vec<Op>) = ops_line
            .char_indices()
            .filter_map(|(i, c)| match c {
                '*' => Some((i, Op::Mul)),
                '+' => Some((i, Op::Add)),
                _ => None,
            })
            .unzip();
        ix.push(ops_line.len() + 1);
        let widths = ix.windows(2).map(|w| w[1] - w[0] - 1).collect::<Vec<_>>();
        let inner_most = widths.iter().map(|w| vec![None; *w]).collect::<Vec<_>>();
        let mut nums_t = Vec::new();
        let ix_width = ix.into_iter().zip(widths).collect::<Vec<_>>();
        for line in lines {
            let mut ns = inner_most.clone();
            for (j, &(i, w)) in ix_width.iter().enumerate() {
                for (k, c) in line[i..i + w].char_indices() {
                    ns[j][k] = c.to_digit(10).map(u64::from);
                }
            }
            nums_t.push(ns);
        }
        let mut nums = vec![vec![Vec::new(); nums_t.len()]; nums_t[0].len()];
        for i in 0..nums_t.len() {
            for (j, n) in nums_t[i].iter().enumerate() {
                nums[j][i].clone_from(n);
            }
        }
        Self { ops, nums }
    }
}

fn sorta_horner(ds: &[Option<u64>]) -> u64 {
    ds.iter().fold(0, |n, m| m.map(|d| 10 * n + d).unwrap_or(n))
}

fn part_1_nums(digits: &[Vec<Option<u64>>]) -> Vec<u64> {
    digits.iter().map(|ds| sorta_horner(ds)).collect()
}

fn part_2_nums(digits: &[Vec<Option<u64>>]) -> Vec<u64> {
    (0..digits[0].len())
        .rev()
        .map(|i| sorta_horner(&digits.iter().map(|ds| ds[i]).collect::<Vec<Option<_>>>()))
        .collect()
}

impl Worksheet {
    fn solve(&self, to_nums: impl Fn(&[Vec<Option<u64>>]) -> Vec<u64>) -> u64 {
        self.ops
            .iter()
            .zip(self.nums.iter())
            .map(|(op, digits)| {
                let xs = to_nums(digits).into_iter();
                match op {
                    Op::Mul => xs.product::<u64>(),
                    Op::Add => xs.sum(),
                }
            })
            .sum()
    }
}

fn main() {
    let input = Worksheet::from(std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", input.solve(part_1_nums));
    println!("{}", input.solve(part_2_nums));
}
