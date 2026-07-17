//! Clock acquisition that works on every target kaish compiles for,
//! including `wasm32-unknown-unknown` (the browser), where std's clock
//! syscalls are unsupported and `SystemTime::now()`/`Instant::now()` panic.
//!
//! The types stay std everywhere they are stored or exposed —
//! `std::time::SystemTime` is freely constructible on wasm
//! (`UNIX_EPOCH + duration`); only *acquisition* needs the platform door.
//! On the browser target that door is [`web-time`], which reads
//! `Date.now()` / `performance.now()` through wasm-bindgen; on every other
//! target these are exactly the std calls, with no extra dependency.
//!
//! [`web-time`]: https://docs.rs/web-time

/// The monotonic clock for internal elapsed-time measurement.
///
/// On the browser target this is `web_time::Instant` (std's is
/// unsupported there); everywhere else it is `std::time::Instant`.
/// `std::time::Instant` has no public constructor, so call sites that
/// measure durations use this alias as their type rather than converting.
#[cfg(all(target_family = "wasm", target_os = "unknown"))]
pub use web_time::Instant;
#[cfg(not(all(target_family = "wasm", target_os = "unknown")))]
pub use std::time::Instant;

/// Wall-clock "now" as a plain `std::time::SystemTime`.
///
/// Identical to `SystemTime::now()` on native targets and WASI; on the
/// browser target it reads the JS clock and rebases onto std's
/// `UNIX_EPOCH` so stored types and signatures stay std everywhere.
pub fn system_now() -> std::time::SystemTime {
    #[cfg(all(target_family = "wasm", target_os = "unknown"))]
    {
        let since_epoch = web_time::SystemTime::now()
            .duration_since(web_time::UNIX_EPOCH)
            .unwrap_or_default();
        std::time::UNIX_EPOCH + since_epoch
    }
    #[cfg(not(all(target_family = "wasm", target_os = "unknown")))]
    {
        std::time::SystemTime::now()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn system_now_tracks_std() {
        // On non-wasm targets system_now IS the std clock; prove they agree
        // to within a generous slop so this can fail if the rebase math is
        // ever wrong on a target where both clocks are live.
        let a = std::time::SystemTime::now();
        let b = system_now();
        let skew = b
            .duration_since(a)
            .unwrap_or_else(|e| e.duration());
        assert!(skew < std::time::Duration::from_secs(1), "skew {skew:?}");
    }

    #[test]
    fn instant_measures_forward() {
        let t0 = Instant::now();
        assert!(t0.elapsed() < std::time::Duration::from_secs(60));
    }
}
