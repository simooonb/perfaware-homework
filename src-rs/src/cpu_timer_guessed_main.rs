use crate::platform_metrics::guess_cpu_freq;

mod platform_metrics;

fn main() {
    let cpu_freq = guess_cpu_freq();
    println!("CPU Freq: {:.4?} (guessed)", cpu_freq);
}
