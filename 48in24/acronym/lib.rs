pub fn abbreviate(phrase: &str) -> String {
    //String::from_iter(phrase.split(&[' ', '-']).map(|s| s.get(0..1).unwrap()))

    let mut flag = true;
    let mut in_word_flag = true;
    let mut result = vec![];
    for c in phrase.chars() {
        if flag {
            if c.is_ascii_alphabetic() {
                result.push(c.to_ascii_uppercase());
                flag = false;
                in_word_flag = false
            }
            continue;
        }

        match c {
            ' ' | '-' => flag = true,
            c if c.is_uppercase() && in_word_flag => {
                result.push(c);
                in_word_flag = false
            }
            c if c.is_lowercase() => in_word_flag = true,
            _ => (),
        }
    }

    String::from_iter(result.into_iter())
}
