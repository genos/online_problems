extern crate chrono;
use chrono::{DateTime, Duration};

pub fn after(d: DateTime<UTC>) -> DateTime<UTC> {
    d + Duration::seconds(1e9 as i64)
}
