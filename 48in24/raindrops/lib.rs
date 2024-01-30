pub fn raindrops(n: u32) -> String {
    let s = String::from_iter(
        [(3, "Pling"), (5, "Plang"), (7, "Plong")]
            .into_iter()
            .map(|(d, s)| if n % d == 0 { s } else { "" }),
    );
    if s.is_empty() {
        format!("{}", n)
    } else {
        s
    }
}
