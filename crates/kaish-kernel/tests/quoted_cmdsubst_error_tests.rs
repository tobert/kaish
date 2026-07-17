//! A syntax error inside a quoted `$()` must be loud, like the unquoted form —
//! not silently turned into literal text. Regression for `parser.rs`
//! (`parse_interpolated_string` used to fall back to literal on parse failure).

#![cfg(feature = "localfs")]
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

// Malformed command substitution inside double quotes: must error at parse time
// (kernel.execute returns Err), the same as the unquoted form does.
#[tokio::test]
async fn malformed_cmdsubst_in_quotes_is_loud() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let res = kernel.execute("echo \"$(if true; echo 1; fi)\"").await;
    assert!(
        res.is_err(),
        "malformed quoted $() must be a loud parse error, got: {res:?}"
    );
}

// The unquoted form already errors — pin that they agree.
#[tokio::test]
async fn malformed_cmdsubst_unquoted_still_loud() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    assert!(kernel
        .execute("echo $(if true; echo 1; fi)")
        .await
        .is_err());
}

// A valid command substitution inside double quotes still works.
#[tokio::test]
async fn valid_cmdsubst_in_quotes_runs() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = kernel.execute("echo \"$(echo ok)\"").await.expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), "ok");
}

// A literal that merely looks dollar-y but isn't a cmdsubst is unaffected.
#[tokio::test]
async fn plain_text_with_cmdsubst_interpolation_runs() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = kernel
        .execute("echo \"pre $(echo mid) post\"")
        .await
        .expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), "pre mid post");
}
