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

// An unterminated `$(` inside a double-quoted string (the closing `"` ends
// the string before the substitution ever finds its own `)`) must be a loud
// parse error naming the missing `)` — never silently accepted by treating
// the rest of the string as the substitution body. Regression for
// `parse_interpolated_string`, which used to fabricate a closing paren: the
// remainder ("echo hi") parses as a valid program on its own, so the missing
// `)` went unnoticed and `--plan` rendered a `)` the user never wrote.
#[tokio::test]
async fn unterminated_cmdsubst_in_quotes_is_loud() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let res = kernel.execute("echo \"pre $(echo hi\"").await;
    let err = res
        .expect_err("unterminated quoted $( must be a loud parse error")
        .to_string();
    assert!(
        err.contains("unterminated command substitution") && err.contains(')'),
        "expected an unterminated-command-substitution message naming the missing `)`, got: {err}"
    );
}

// The unquoted form already errors on the same input shape — pin that the
// quoted and unquoted paths agree, and that the message names the missing `)`.
#[tokio::test]
async fn unterminated_cmdsubst_unquoted_matches_message() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let err = kernel
        .execute("echo $(echo hi")
        .await
        .expect_err("unterminated unquoted $( must be a loud parse error")
        .to_string();
    assert!(
        err.contains("unterminated command substitution") && err.contains(')'),
        "expected an unterminated-command-substitution message naming the missing `)`, got: {err}"
    );
}

// A closed quoted substitution is unaffected by the unterminated check —
// keep this passing so the fix cannot regress the common case.
#[tokio::test]
async fn terminated_cmdsubst_in_quotes_still_runs() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = kernel
        .execute("echo \"pre $(echo hi)\"")
        .await
        .expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), "pre hi");
}

// A remainder that fails to parse on its own (not just "runs off the end")
// must stay loud too — pins that the fix doesn't regress the existing
// parse-failure path this file's other tests cover.
#[tokio::test]
async fn unterminated_and_unparseable_cmdsubst_in_quotes_is_loud() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let res = kernel.execute("echo \"pre $(;;\"").await;
    assert!(
        res.is_err(),
        "unterminated + unparseable quoted $( must be a loud parse error, got: {res:?}"
    );
}

// Two substitutions in one string, the second left unterminated — the first,
// valid one must not mask the second's missing `)`.
#[tokio::test]
async fn second_unterminated_cmdsubst_in_quotes_is_loud() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let res = kernel.execute("echo \"a $(echo b) c $(echo d\"").await;
    assert!(
        res.is_err(),
        "a later unterminated quoted $( must still be a loud parse error, got: {res:?}"
    );
}
