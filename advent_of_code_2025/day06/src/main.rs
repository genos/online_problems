enum Op {
    Mul,
    Add,
}

struct WS {
    ops: Vec<Op>,
    nums: Vec<Vec<Vec<Option<u64>>>>,
}

impl From<String> for WS {
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
                nums[j][i] = n.to_vec();
            }
        }
        Self { ops, nums }
    }
}

impl WS {
    fn part_1(&self) -> u64 {
        self.ops
            .iter()
            .zip(self.nums.iter())
            .map(|(op, digits)| {
                let xs = digits.iter().map(|ds| {
                    ds.iter()
                        .fold(0, |acc, m| m.map(|d| 10 * acc + d).unwrap_or(acc))
                });
                match op {
                    Op::Mul => xs.product::<u64>(),
                    Op::Add => xs.sum(),
                }
            })
            .sum()
    }
}

fn main() {
    let input = WS::from(std::fs::read_to_string("input.txt").expect("file"));
    println!("{}", input.part_1());
}
