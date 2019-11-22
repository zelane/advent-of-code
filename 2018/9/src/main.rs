use std::fs;
use regex::Regex;
use std::collections::{VecDeque};


fn main() -> std::io::Result<()> {
    let file = fs::read_to_string("input.txt")?;
    let re = Regex::new(r"([0-9]+)").unwrap();
    
    let inputs: Vec<usize> = re.captures_iter(&file)
        .map(|cap| cap[0].parse::<usize>().unwrap())
        .collect();

    let answer_1 = play(inputs[0], inputs[1]);
    println!("{}", answer_1);

    let answer_2 = play(inputs[0], inputs[1]*100);
    println!("{}", answer_2);

    Ok(())
}

fn play(player_count:usize, max_m:usize) -> usize {
    let mut players: Vec<usize> = Vec::new();
    for _ in 0..player_count {
        players.push(0);
    }
    let mut circle: VecDeque<usize> = VecDeque::new();
    circle.push_back(0);

    for m in 1..max_m + 1 {
        if m % 23 == 0 {
            let current_player: usize = (m-1) % player_count;
            circle.rotate_right(7);
            let removed = circle.pop_back().unwrap();
            players[current_player] += m + removed;
            circle.rotate_left(1);
            continue
        }
        circle.rotate_left(1);
        circle.push_back(m);
    }

    return *players.iter().max().unwrap();
}
