use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::collections::{HashSet, VecDeque};

fn main() -> std::io::Result<()> {
    let file = File::open("src/input.txt")?;
    let reader = BufReader::new(file);
    let mut values = Vec::new();
    
    for line in reader.lines() {
        let line = line.unwrap();
        let (sign, value) = line.split_at(1);
        let mut value: i32 = value.parse().unwrap();

        if sign == "-" {
           value *= -1;
        }
        values.push(value)
    }

    let sum: i32 = values.iter().sum();
    println!("{}", sum);

    let mut values = VecDeque::from(values);
    let mut frequencies = HashSet::new();
    let mut freq = 0;

    loop {
        let cur_val = values.pop_front().unwrap();
        freq += cur_val;

        if frequencies.contains(&freq) {
            println!("{}", freq);
            break;
        }
        frequencies.insert(freq);
        values.push_back(cur_val); 
    }

    Ok(())
}
