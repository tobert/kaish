//! GH #194: control structures (`if`/`for`/`while`/`case`) inside an
//! UNQUOTED `$(...)` actually RUN, not just parse.
//!
//! The parser-level fix (route C: the body is balance-captured then parsed
//! through the full program grammar, `parser.rs`) is pinned by unit tests in
//! `parser.rs` itself. These pin the runtime half: `Expr::CommandSubst(Vec<Stmt>)`
//! and `execute_block_capturing` (`kernel.rs`) already accepted arbitrary
//! statements before this fix — only the unquoted grammar production
//! rejected them — so nothing downstream needed to change, but that claim is
//! only as good as a test that actually executes the construct.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

#[tokio::test]
async fn unquoted_for_loop_inside_cmdsubst_runs() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = kernel
        .execute(r#"echo $(for f in a b; do echo $f; done)"#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "a\nb");
}

#[tokio::test]
async fn unquoted_while_loop_inside_cmdsubst_runs() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = kernel
        .execute(r#"x=$(i=0; while [[ $i -lt 3 ]]; do echo $i; i=$((i+1)); done); echo "[$x]""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "[0\n1\n2]");
}

#[tokio::test]
async fn unquoted_if_else_inside_cmdsubst_runs() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = kernel
        .execute(r#"x=$(if true; then echo yep; else echo nope; fi); echo "[$x]""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "[yep]");
}

#[tokio::test]
async fn unquoted_case_inside_cmdsubst_runs() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = kernel
        .execute(r#"x=$(case dog in cat) echo meow;; dog) echo woof;; esac); echo "[$x]""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "[woof]");
}

#[tokio::test]
async fn unquoted_nested_cmdsubst_with_inner_control_structure_runs() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = kernel
        .execute(r#"echo $(echo $(for f in a b; do echo $f; done))"#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    // Whitespace, not newlines, at the outer echo: the inner $() result is
    // one string ("a\nb") interpolated as a single word to the outer echo.
    assert_eq!(r.text_out().trim(), "a\nb");
}

/// Bash-parity: the quoted form (`"$(...)"`) already ran control structures
/// before this fix — pinned so route C cannot regress it while fixing the
/// unquoted form.
#[tokio::test]
async fn quoted_for_loop_inside_cmdsubst_still_runs() {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = kernel
        .execute(r#"out="$(for f in a b; do echo $f; done)"; echo "$out""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "a\nb");
}
