use crate::platform_metrics::{get_os_timer_freq, read_os_timer};

mod platform_metrics;

fn main() {
    let os_freq = get_os_timer_freq();
    println!("OS Freq: {}", os_freq);

    let os_start: u64 = read_os_timer();
    let mut os_end: u64 = 0;
    let mut os_elapsed: u64 = 0;

    while os_elapsed < os_freq {
        os_end = read_os_timer();
        os_elapsed = os_end - os_start;
    }

    println!("OS Timer: {} -> {} = {} elapsed", os_start, os_end, os_elapsed);
    println!("OS Seconds: {:.4?}", os_elapsed as f64 / os_freq as f64);
}
