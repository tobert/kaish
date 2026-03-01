//! Confirmation nonce store for dangerous operations.
//!
//! Used by the latch system (`set -o latch`) to gate destructive commands
//! behind a nonce-based confirmation flow. Nonces are time-limited and
//! reusable within their TTL for idempotent retries.

use std::collections::HashMap;
use std::hash::{BuildHasher, Hasher};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant, SystemTime};

/// A store for confirmation nonces with TTL-based expiration.
///
/// Nonces are 8-character hex strings that gate dangerous operations.
/// They remain valid until their TTL expires — not consumed on validation —
/// making operations idempotent: a retried `rm --confirm=abc123 bigdir/`
/// works if the nonce hasn't expired.
#[derive(Clone)]
pub struct NonceStore {
    inner: Arc<Mutex<NonceStoreInner>>,
    ttl: Duration,
}

struct NonceStoreInner {
    /// Map from nonce string to (created_at, description).
    nonces: HashMap<String, (Instant, String)>,
}

impl NonceStore {
    /// Create a new nonce store with the default TTL (60 seconds).
    pub fn new() -> Self {
        Self::with_ttl(Duration::from_secs(60))
    }

    /// Create a new nonce store with a custom TTL.
    pub fn with_ttl(ttl: Duration) -> Self {
        Self {
            inner: Arc::new(Mutex::new(NonceStoreInner {
                nonces: HashMap::new(),
            })),
            ttl,
        }
    }

    /// Issue a new nonce for the given description.
    ///
    /// Returns an 8-character hex string. Opportunistically GCs expired nonces.
    pub fn issue(&self, description: impl Into<String>) -> String {
        let nonce = generate_nonce();
        let now = Instant::now();
        let ttl = self.ttl;

        #[allow(clippy::expect_used)]
        let mut inner = self.inner.lock().expect("nonce store poisoned");

        // Opportunistic GC: remove expired nonces
        inner.nonces.retain(|_, (created, _)| now.duration_since(*created) < ttl);

        inner.nonces.insert(nonce.clone(), (now, description.into()));
        nonce
    }

    /// Validate a nonce, returning the description if valid.
    ///
    /// Does NOT consume the nonce — it stays valid until TTL expires.
    pub fn validate(&self, nonce: &str) -> Result<String, String> {
        let now = Instant::now();
        let ttl = self.ttl;

        #[allow(clippy::expect_used)]
        let inner = self.inner.lock().expect("nonce store poisoned");

        match inner.nonces.get(nonce) {
            Some((created, description)) => {
                if now.duration_since(*created) < ttl {
                    Ok(description.clone())
                } else {
                    Err("nonce expired".to_string())
                }
            }
            None => Err("invalid nonce".to_string()),
        }
    }

    /// Get the TTL for nonces in this store.
    pub fn ttl(&self) -> Duration {
        self.ttl
    }
}

impl Default for NonceStore {
    fn default() -> Self {
        Self::new()
    }
}

/// Generate an 8-character hex nonce using RandomState + SystemTime.
fn generate_nonce() -> String {
    let hasher_state = std::collections::hash_map::RandomState::new();
    let mut hasher = hasher_state.build_hasher();

    // Mix in current time for uniqueness
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default();
    hasher.write_u128(now.as_nanos());

    // Mix in a second RandomState for additional entropy
    let hasher_state2 = std::collections::hash_map::RandomState::new();
    let mut hasher2 = hasher_state2.build_hasher();
    hasher2.write_u64(0xdeadbeef);
    hasher.write_u64(hasher2.finish());

    format!("{:08x}", hasher.finish() as u32)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn issue_and_validate() {
        let store = NonceStore::new();
        let nonce = store.issue("delete /tmp/important");
        assert_eq!(nonce.len(), 8);
        assert!(nonce.chars().all(|c| c.is_ascii_hexdigit()));

        let result = store.validate(&nonce);
        assert_eq!(result, Ok("delete /tmp/important".to_string()));
    }

    #[test]
    fn idempotent_reuse() {
        let store = NonceStore::new();
        let nonce = store.issue("rm bigdir/");

        let first = store.validate(&nonce);
        let second = store.validate(&nonce);
        assert!(first.is_ok());
        assert!(second.is_ok());
        assert_eq!(first, second);
    }

    #[test]
    fn expired_nonce_fails() {
        let store = NonceStore::with_ttl(Duration::from_millis(0));
        let nonce = store.issue("ephemeral");

        // With 0ms TTL, nonce is immediately expired
        std::thread::sleep(Duration::from_millis(1));
        let result = store.validate(&nonce);
        assert_eq!(result, Err("nonce expired".to_string()));
    }

    #[test]
    fn invalid_nonce_fails() {
        let store = NonceStore::new();
        let result = store.validate("bogus123");
        assert_eq!(result, Err("invalid nonce".to_string()));
    }

    #[test]
    fn nonces_are_unique() {
        let store = NonceStore::new();
        let a = store.issue("first");
        let b = store.issue("second");
        assert_ne!(a, b);
    }

    #[test]
    fn clone_shares_state() {
        let store = NonceStore::new();
        let cloned = store.clone();
        let nonce = store.issue("shared");

        let result = cloned.validate(&nonce);
        assert!(result.is_ok());
    }

    #[test]
    fn gc_cleans_expired() {
        let store = NonceStore::with_ttl(Duration::from_millis(10));
        let old_nonce = store.issue("old");

        std::thread::sleep(Duration::from_millis(20));

        // This issue() triggers GC
        let _new = store.issue("new");

        // Old nonce should be gone (GC'd)
        let result = store.validate(&old_nonce);
        assert!(result.is_err());
    }
}
