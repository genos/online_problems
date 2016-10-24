pub fn hello(name: Option<&str>) -> String {
    format!("Hello, {}!", match name {
        Some(n) => n,
        None => "World"
    })
}
