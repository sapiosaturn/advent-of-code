use std::collections::HashMap;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let file_contents: String = match fs::read_to_string(filename) {
        Ok(contents) => contents,
        Err(e) => {
            println!("Error reading input file: {e}.");
            String::new()
        }
    };
    let mut first_column: Vec<i32> = Vec::new();
    let mut second_column: Vec<i32> = Vec::new();

    for line in file_contents.split('\n') {
        let two_numbers: Vec<i32> = line
            .split("   ")
            .map(|x| match x.trim().parse::<i32>() {
                Ok(number) => number,
                Err(_) => 0, // any lines without info
            })
            .collect(); // three spaces separating numbers
        if two_numbers.len() != 2 {
            // if more/less than two numbers parsed
            break;
        }
        first_column.push(two_numbers[0]);
        second_column.push(two_numbers[1]);
    }
    first_column.sort();
    second_column.sort();

    let mut frequencies: HashMap<i32, i32> = HashMap::new();
    let mut answer = 0;

    for item in second_column.iter() {
        frequencies
            .entry(*item)
            .and_modify(|count| *count += 1)
            .or_insert(1);
    }
    for item in first_column.iter() {
        let frequency = match frequencies.get(item) {
            None => 0,
            Some(f) => *f,
        };
        answer += *item * frequency;
    }

    println!("{}", answer);
}
