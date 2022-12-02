use std::{env, fs};

use aoc22::run_day;

fn result_main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = env::args().skip(1).collect();
    if args.is_empty() {
        return Err("Missing day to run".into());
    }

    if args.len() < 2 {
        return Err("Missing file to run".into());
    }

    let day: usize = args[0].parse()?;
    let filename = &args[1];
    let input = fs::read_to_string(filename)?;

    println!("{}", run_day(day, &input));

    Ok(())
}

fn main() {
    if let Err(e) = result_main() {
        eprintln!("Error: {}", e);
    }
}
