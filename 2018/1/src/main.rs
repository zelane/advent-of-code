use std::fs::File;
use std::io::{prelude::*, BufReader};

fn main() -> std::io::Result<()> {
    let file = File::open("src/input.txt")?;
    let reader = BufReader::new(file);

    let mut freq = 0;

    for line in reader.lines() {
        let mut line = line.unwrap();
        let value = line.split_off(1);
        let mut int: i32 = value.parse().unwrap();

        if line == "-" {
            int = -1 * int;
        }
        freq += int;
    }
    println!("{}", freq);
    Ok(())
}
