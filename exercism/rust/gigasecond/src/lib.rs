extern crate chrono;
use chrono::{DateTime, Duration, UTC};

pub fn after(d: DateTime<UTC>) -> DateTime<UTC> {
    d + Duration::seconds(1_000_000_000)
}
