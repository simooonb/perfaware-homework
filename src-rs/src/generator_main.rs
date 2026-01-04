mod haversine_distance;
mod coordinates_generator;

use std::time::Instant;
use clap::Parser;
use crate::coordinates_generator::generate;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    mode: String,

    #[arg(short, long)]
    seed: u64,

    #[arg(short, long)]
    pairs: u64
}

fn main() {
    let args = Args::parse();

    let start = Instant::now();
    let result = generate(&args.mode, args.seed, args.pairs);
    let elapsed = start.elapsed();

    println!("Method: {}", args.mode);
    println!("Random seed: {}", args.seed);
    println!("Pair count: {}", args.pairs);
    println!("Expected sum: {}", result);
    println!("Elapsed time: {:.3?}", elapsed);
}
