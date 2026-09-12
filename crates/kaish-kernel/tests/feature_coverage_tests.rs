//! A per-crate test run must not pass with its process and filesystem suites
//! compiled out.
//!
//! Several test files and `kernel.rs`'s unit tests are gated on the
//! `subprocess` and `localfs` features, and `subprocess` is not a default
//! feature. `cargo test -p kaish-kernel` compiled those suites to nothing and
//! still exited 0. This test is never gated, so that run fails with the fix
//! named instead.

#[test]
fn process_and_filesystem_suites_are_built() {
    // A deliberately reduced build (CI's no-default-features leg) opts out.
    if std::env::var_os("KAISH_ALLOW_REDUCED_TESTS").is_some() {
        return;
    }
    assert!(
        cfg!(feature = "subprocess") && cfg!(feature = "localfs"),
        "kaish-kernel's subprocess and localfs test suites were compiled out. \
         Run `cargo test -p kaish-kernel --features subprocess,localfs`, or set \
         KAISH_ALLOW_REDUCED_TESTS=1 for a deliberately reduced build such as \
         --no-default-features."
    );
}
