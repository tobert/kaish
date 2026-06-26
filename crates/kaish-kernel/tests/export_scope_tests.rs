//! `export NAME=VALUE` inside a function must publish the value to the shared
//! (global) scope, not the function's frame — matching bash and kaish's
//! documented "functions execute in shared scope" rule (docs/LANGUAGE.md).
//!
//! Regression: `export` wrote the value via `set` (innermost frame), so the
//! function's pushed frame was popped on return and the value vanished — even
//! though the export *marking* (a flat set) survived, leaving an exported name
//! with no value. See docs/issues.md P2 "Interpreter item".

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("kernel")
        .into_arc()
}

#[tokio::test]
async fn export_assignment_in_function_persists_globally() {
    let k = setup().await;
    let r = k
        .execute("greet() { export FOO=bar; }\ngreet\necho \"[$FOO]\"")
        .await
        .expect("ok");
    assert_eq!(
        r.text_out().trim(),
        "[bar]",
        "export FOO=bar in a function should publish FOO to the shared scope"
    );
}

#[tokio::test]
async fn export_assignment_in_function_stays_exported() {
    let k = setup().await;
    // After the function returns, FOO is both set and marked exported, so it
    // shows up in `export -p` with its value.
    let r = k
        .execute("greet() { export FOO=bar; }\ngreet\nexport -p")
        .await
        .expect("ok");
    assert!(
        r.text_out().contains("declare -x FOO=\"bar\""),
        "FOO should be listed as exported with its value; got: {}",
        r.text_out()
    );
}

#[tokio::test]
async fn plain_assignment_in_function_already_persists() {
    // Control: a plain assignment already uses shared-scope semantics. This
    // pins that the export fix matches the existing plain-assignment behavior.
    let k = setup().await;
    let r = k
        .execute("greet() { BAZ=qux; }\ngreet\necho \"[$BAZ]\"")
        .await
        .expect("ok");
    assert_eq!(r.text_out().trim(), "[qux]");
}
