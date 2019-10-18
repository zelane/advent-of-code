use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::collections::HashSet;

fn main() -> std::io::Result<()> {

    let mut frequencies = HashSet::new();
    let mut freq = 0;

    let mut run = true;
    while run {
        let file = File::open("src/input.txt")?;
        let reader = BufReader::new(file);
        for line in reader.lines() {
            let mut line = line.unwrap();
            let value = line.split_off(1);
            let mut int: i32 = value.parse().unwrap();

            if line == "-" {
                int = -1 * int;
            }
            freq += int;

            if frequencies.contains(&freq) {
                println!("{}", freq);
                run = false;
                break;
            }

            frequencies.insert(freq);
        }
    }

    Ok(())
}
