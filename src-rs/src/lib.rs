use std::arch::asm;

#[unsafe(no_mangle)]
pub extern "C" fn get_cycles() -> u64 {
    let val: u64;
    unsafe {
        #[cfg(target_arch = "aarch64")]
        asm!("mrs {0}, cntvct_el0", out(reg) val, options(nomem, nostack));

        #[cfg(target_arch = "x86_64")]
        {
            let lo: u32;
            let hi: u32;
            asm!("rdtsc", out("eax") lo, out("edx") hi);
            val = ((hi as u64) << 32) | (lo as u64);
        }
    }
    val
}
