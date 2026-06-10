//! Shared byte budget for memory-resident filesystems.
//!
//! Counting and limiting are different concerns (see
//! `docs/kaish-overlayfs.md`, "Byte accounting"): every memory-resident
//! filesystem counts its own bytes unconditionally via
//! [`Filesystem::resident_bytes`](crate::Filesystem::resident_bytes); a
//! `ByteBudget` is the optional, shared *limit*. One `Arc<ByteBudget>` handed
//! to several filesystems (a kernel's `/` scratch, a workspace overlay's
//! upper and bases) gives one number — and one loud failure mode — per
//! consumer.

use std::io;
use std::sync::atomic::{AtomicU64, Ordering};

/// A shared cap on memory-resident bytes.
///
/// Cloned via `Arc` into every filesystem that should draw from the same
/// pool. Exceeding the limit fails the write loudly, ENOSPC-style — an
/// in-band error a model reads and adapts to; fail loud over quietly eating
/// RAM.
#[derive(Debug)]
pub struct ByteBudget {
    used: AtomicU64,
    limit: u64,
    /// Names the budget in errors so the failure points at the knob.
    label: String,
}

impl ByteBudget {
    /// A budget of `limit` bytes labeled "memory" in error messages.
    pub fn new(limit: u64) -> Self {
        Self::labeled(limit, "memory")
    }

    /// A budget whose errors name `label` (e.g. a config knob like
    /// `vfs-budget`), so the failure tells the reader what to raise.
    pub fn labeled(limit: u64, label: impl Into<String>) -> Self {
        Self {
            used: AtomicU64::new(0),
            limit,
            label: label.into(),
        }
    }

    /// The configured cap in bytes.
    pub fn limit(&self) -> u64 {
        self.limit
    }

    /// Bytes currently charged against the budget.
    pub fn used(&self) -> u64 {
        self.used.load(Ordering::Acquire)
    }

    /// Bytes still available.
    pub fn remaining(&self) -> u64 {
        self.limit.saturating_sub(self.used())
    }

    /// Reserve `bytes` against the budget, or fail loudly without charging.
    pub fn try_charge(&self, bytes: u64) -> io::Result<()> {
        let exhausted = |current: u64| {
            io::Error::new(
                io::ErrorKind::StorageFull,
                format!(
                    "{} budget exhausted: {} bytes used + {} requested exceeds the {} byte limit",
                    self.label, current, bytes, self.limit
                ),
            )
        };
        let mut current = self.used.load(Ordering::Relaxed);
        loop {
            // An arithmetic overflow is past any limit by definition.
            let next = current.checked_add(bytes).ok_or_else(|| exhausted(current))?;
            if next > self.limit {
                return Err(exhausted(current));
            }
            match self
                .used
                .compare_exchange_weak(current, next, Ordering::AcqRel, Ordering::Relaxed)
            {
                Ok(_) => return Ok(()),
                Err(actual) => current = actual,
            }
        }
    }

    /// Return `bytes` to the budget.
    ///
    /// Panics if it would underflow: crediting more than was charged means
    /// the caller's accounting is wrong, and a limiter that under-reports is
    /// worse than no limiter — crash over drift.
    pub fn credit(&self, bytes: u64) {
        let previous = self.used.fetch_sub(bytes, Ordering::AcqRel);
        assert!(
            previous >= bytes,
            "ByteBudget '{}' underflow: credited {} bytes with only {} charged — accounting bug",
            self.label,
            bytes,
            previous
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_charge_and_credit() {
        let budget = ByteBudget::new(100);
        budget.try_charge(60).unwrap();
        assert_eq!(budget.used(), 60);
        assert_eq!(budget.remaining(), 40);
        budget.credit(20);
        assert_eq!(budget.used(), 40);
    }

    #[test]
    fn test_exceeding_fails_loudly_without_charging() {
        let budget = ByteBudget::labeled(100, "vfs-budget");
        budget.try_charge(90).unwrap();
        let error = budget.try_charge(11).unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::StorageFull);
        assert!(error.to_string().contains("vfs-budget"));
        assert!(error.to_string().contains("100"));
        // The failed charge left the budget untouched.
        assert_eq!(budget.used(), 90);
        budget.try_charge(10).unwrap();
    }

    #[test]
    fn test_overflow_treated_as_full() {
        let budget = ByteBudget::new(u64::MAX);
        budget.try_charge(u64::MAX - 1).unwrap();
        let error = budget.try_charge(u64::MAX).unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::StorageFull);
    }

    #[test]
    #[should_panic(expected = "underflow")]
    fn test_credit_underflow_panics() {
        let budget = ByteBudget::new(100);
        budget.try_charge(10).unwrap();
        budget.credit(11);
    }
}
