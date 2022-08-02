use time::{Duration, PrimitiveDateTime};

pub fn after(d: PrimitiveDateTime) -> PrimitiveDateTime {
    d + Duration::seconds(1_000_000_000)
}
