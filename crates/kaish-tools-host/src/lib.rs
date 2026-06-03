//! kaish's host-introspection tool bundle.
//!
//! This crate keeps `procfs` out of `kaish-kernel`. Today it holds the `ps`
//! builtin; other host-capability tools can join it. `ps` reads `/proc`, so it
//! is Linux-only and compiled out elsewhere.
//!
//! The kernel registers [`Ps`] behind its `host` feature (and `target_os =
//! "linux"`). Hermetic identity tools like `uname` deliberately stay in the
//! kernel — they need no host crate and `uname` is useful in the sandbox too.

#[cfg(target_os = "linux")]
mod ps;
#[cfg(target_os = "linux")]
pub use ps::Ps;
