mod haversine_distance;
mod coordinates_processor;
mod platform_metrics;

use std::fs;
use std::time::Instant;
use clap::Parser;
use crate::coordinates_processor::process;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    input: String,

    #[arg(short, long)]
    expected: String,
}

fn main() {
    let args = Args::parse();

    let start = Instant::now();
    let result = process(&args.input);
    let elapsed = start.elapsed();

    let expected = fs::read_to_string(args.expected).expect("Should have been able to read the file").parse::<f64>().unwrap();

    println!("Result: {}", result);
    println!("Expected sum: {}", expected);
    println!("Difference: {}", result - expected);
    println!("Elapsed time: {:.3?}", elapsed);
}
