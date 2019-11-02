use std::fs::File;
use std::io::{prelude::*, BufReader};

fn main() -> std::io::Result<()> {
    part_1();
    part_2();
    return Ok(());
}

fn part_1() {
    let file = File::open("src/input.txt").unwrap();
    let reader = BufReader::new(file);

    let mut twos = 0;
    let mut threes = 0;

    for line in reader.lines() {
        let chars: Vec<char> = line.unwrap().chars().collect();

        let mut has_twos = false;
        let mut has_threes = false;

        for _char in chars.clone() {
            let count: usize = chars.iter().filter(|&n| *n == _char).count();
            
            if count == 2usize {
                has_twos = true;
            }
            else if count == 3usize {
                has_threes = true;
            }
        }

        if has_twos {
            twos += 1;
        }
        if has_threes {
            threes += 1;
        }
    }

    println!("{}", twos * threes);
}

fn part_2() {
    
    let file = File::open("src/input.txt").unwrap();
    let reader = BufReader::new(file);
    
    let lines: Vec<String> = reader.lines().filter_map(Result::ok).collect();
    
    for line in lines.iter() {
        for line2 in lines.iter() {
            // println!("Comparing {} to {}", line, line2);
            let mut diff = 0;
            let mut result =  String::with_capacity(line.len());
            for (i, c) in line.chars().enumerate() {
                if c != line2.chars().nth(i).unwrap() {
                    diff += 1;
                }
                else {
                    result.push(c);
                }
            }
            // println!("{}", diff);
            if diff == 1 {
                println!("{} {}", line, line2);
                println!("{}", result);
                return
            }
        }
    }
}