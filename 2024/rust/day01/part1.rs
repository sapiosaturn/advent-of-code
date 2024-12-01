use std::env;
use std::fs;
use std::iter::zip;

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

    let answer: i32 = zip(first_column.iter(), second_column.iter())
        .map(|x| (x.0 - x.1).abs())
        .sum();

    println!("{}", answer);
}
