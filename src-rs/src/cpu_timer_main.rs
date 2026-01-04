use crate::platform_metrics::{read_cpu_freq, read_cpu_timer};

mod platform_metrics;

fn main() {
    let cpu_freq = read_cpu_freq();
    println!("CPU Freq: {}", cpu_freq);

    let cpu_start: u64 = read_cpu_timer();
    let mut cpu_end: u64 = 0;
    let mut cpu_elapsed: u64 = 0;

    while cpu_elapsed < cpu_freq {
        cpu_end = read_cpu_timer();
        cpu_elapsed = cpu_end - cpu_start;
    }
    
    println!("CPU Timer: {} -> {} = {} elapsed", cpu_start, cpu_end, cpu_elapsed);
    println!("CPU Seconds: {:.4?}", cpu_elapsed as f64 / cpu_freq as f64);
}
