#[derive(Debug, Clone)]
pub struct PascalsTriangle {
    _rows: Vec<Vec<u32>>,
}

impl PascalsTriangle {
    pub fn new(row_count: u32) -> Self {
        PascalsTriangle { _rows: create(row_count as usize) }
    }
    pub fn rows(&self) -> Vec<Vec<u32>> {
        self._rows.clone()
    }
}

fn create(row_count: usize) -> Vec<Vec<u32>> {
    if row_count == 0 {
        vec![]
    } else {
        let mut rs: Vec<Vec<u32>> = vec![vec![1]];
        for i in 1..row_count {
            let mut r = rs[i - 1].clone();
            r.insert(0, 1);
            for j in 1..r.len() - 1 {
                r[j] = r[j] + r[j + 1];
            }
            rs.push(r);
        }
        rs
    }
}
