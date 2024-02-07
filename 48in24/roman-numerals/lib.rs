use std::fmt::{Display, Formatter, Result};

use std::collections::HashMap;

fn int_to_roman(number: u32) -> String {
    let table: HashMap<u32, &str> = [
        (1, "I"),
        (4, "IV"),
        (5, "V"),
        (9, "IX"),
        (10, "X"),
        (40, "XL"),
        (50, "L"),
        (90, "XC"),
        (100, "C"),
        (400, "CD"),
        (500, "D"),
        (900, "CM"),
        (1000, "M"),
    ]
    .iter()
    .cloned()
    .collect();

    let order = vec![1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
    let mut result = String::new();
    let mut num = number;
    loop {
        if num == 0 {
            break;
        }

        if table.contains_key(&num) {
            result = result + table.get(&num).unwrap();
            break;
        }

        for v in &order {
            if num > *v {
                for _i in 0..(num / v) {
                    result = result + table.get(&v).unwrap();
                }
                num %= v;
                break;
            }
        }
    }

    result
}

pub struct Roman {
    a: u32,
}

impl Display for Roman {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", int_to_roman(self.a))
    }
}

impl From<u32> for Roman {
    fn from(num: u32) -> Self {
        Self { a: num }
    }
}
