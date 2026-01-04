use std::arch::asm;
use std::time::{SystemTime, UNIX_EPOCH};

pub fn get_os_timer_freq() -> u64 {
    1000000
}

// or Instant?
pub fn read_os_timer() -> u64 {
    let start = SystemTime::now();
    let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();

    get_os_timer_freq() * since_the_epoch.as_secs() + since_the_epoch.subsec_micros() as u64
}

#[inline]
pub fn read_cpu_timer() -> u64 {
    let val: u64;
    unsafe {
        asm!("mrs {0}, cntvct_el0", out(reg) val, options(nomem, nostack));
    }
    val
}

#[inline]
pub fn read_cpu_freq() -> u64 {
    let val: u64;
    unsafe {
        asm!("mrs {0}, cntfrq_el0", out(reg) val, options(nomem, nostack));
    }
    val
}

pub fn guess_cpu_freq() -> u64 {
    let millis_to_wait: u64 = 1000;
    let os_freq = get_os_timer_freq();

    let cpu_start: u64 = read_cpu_timer();
    let os_start: u64 = read_os_timer();
    let mut os_end: u64 = 0;
    let mut os_elapsed: u64 = 0;
    let os_wait_time: u64 = os_freq * millis_to_wait / 1000;

    while os_elapsed < os_wait_time {
        os_end = read_os_timer();
        os_elapsed = os_end - os_start;
    }

    let cpu_end = read_cpu_timer();
    let cpu_elapsed = cpu_end - cpu_start;

    os_freq * cpu_elapsed / os_elapsed
}
