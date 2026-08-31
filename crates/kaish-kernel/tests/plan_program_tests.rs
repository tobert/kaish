//! `plan_program`: the statement-metadata surface an embedder analyzes a
//! program with.
//!
//! Planning executes nothing and resolves nothing: `${HOME}` and `$(...)`
//! read back exactly as written. `free_variables` names what a statement
//! reads, so an embedder can peek live session state with `Kernel::get_var`
//! before deciding whether to run it; `bound_variables` names what it
//! writes, and a name that is both lands bound, never free.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

// KernelConfig::repl() mounts the real filesystem.
#![cfg(feature = "localfs")]

use kaish_kernel::{plan_program, Kernel, KernelConfig};

/// A kernel rooted in its own directory. Planning touches nothing, but the
/// peek-loop test below runs real assignments through it.
fn kernel_at(dir: &std::path::Path) -> Kernel {
    Kernel::new(KernelConfig::repl().with_cwd(dir.to_path_buf()).with_trash(false))
        .expect("kernel")
}

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("plan-program-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// The free function needs no kernel at all: same source, same plans, no
/// filesystem anywhere in sight.
#[test]
fn plan_program_needs_no_kernel() {
    let plans = plan_program("echo before\nrm f.txt\necho after").expect("parses");
    assert_eq!(
        plans
            .iter()
            .map(|p| (p.index, p.plan.rendered.as_str()))
            .collect::<Vec<_>>(),
        vec![(0, "echo before"), (1, "rm f.txt"), (2, "echo after")],
    );
}

/// Nothing executes during planning: the statement that would delete the
/// file is planned, and the file is still there.
#[tokio::test]
async fn planning_executes_nothing() {
    let dir = tempdir();
    let target = dir.path().join("f.txt");
    std::fs::write(&target, "keep me").expect("write");
    let kernel = kernel_at(dir.path());

    let plans = kernel.plan_program("rm f.txt").expect("plans");
    assert_eq!(plans[0].plan.commands[0].name, "rm");
    assert!(target.exists(), "planning must not run the statement");
}

/// The plan names the session variables a statement reads, and the peek
/// loop the metadata surface promises works against live state: plan, then
/// `get_var` each free variable, and judge with the value in hand.
#[tokio::test]
async fn free_variables_feed_the_get_var_peek_loop() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    kernel
        .execute("TARGET=precious.txt")
        .await
        .expect("assignment");

    let plans = kernel.plan_program("rm ${TARGET}").expect("plans");
    assert_eq!(plans[0].plan.free_variables, vec!["TARGET".to_string()]);
    assert!(plans[0].plan.bound_variables.is_empty());

    let value = kernel
        .get_var(&plans[0].plan.free_variables[0])
        .await
        .expect("TARGET is set");
    assert_eq!(value, kaish_types::Value::String("precious.txt".to_string()));
}

/// A name the statement itself binds is never listed free — peeking session
/// state for a `for` variable would judge the statement against a value the
/// statement replaces.
#[test]
fn bound_names_never_read_as_free() {
    let plans =
        plan_program("for f in $(ls ${DIR}); do rm $f; done").expect("parses");
    assert_eq!(plans[0].plan.free_variables, vec!["DIR".to_string()]);
    assert_eq!(plans[0].plan.bound_variables, vec!["f".to_string()]);
}

// ── A `$(...)` reached only through `$(( ))` still plans its own commands ──
//
// `--plan` promises every command a statement may run is present in
// `Plan::commands` — an embedder approves against that list, so a command
// missing from it is a command that runs unapproved. `read_arith_expansion`'s
// `CommandSubst` arm used to keep only `inner.reads` and drop
// `inner.commands`, so a `$(...)` reached only through arithmetic never
// reached the plan at all.

#[test]
fn a_command_substitution_inside_arithmetic_plans_its_own_commands() {
    let plans = plan_program("echo $((1 + $(touch PROBE; echo 1)))").expect("parses");
    let names: Vec<&str> = plans[0].plan.commands.iter().map(|c| c.name.as_str()).collect();
    assert_eq!(
        names,
        vec!["echo", "touch", "echo"],
        "both commands inside $(( $(...) )) must be planned, in source order"
    );
}

#[test]
fn a_command_substitution_inside_a_based_expansion_plans_its_own_command() {
    let plans = plan_program("echo $((10#$(printf 42)))").expect("parses");
    let names: Vec<&str> = plans[0].plan.commands.iter().map(|c| c.name.as_str()).collect();
    assert_eq!(names, vec!["echo", "printf"]);
}

#[test]
fn a_command_substitution_inside_an_arithmetic_default_plans_its_own_command() {
    let plans = plan_program("echo $((${cnt:-$(printf 9)}))").expect("parses");
    let names: Vec<&str> = plans[0].plan.commands.iter().map(|c| c.name.as_str()).collect();
    assert_eq!(names, vec!["echo", "printf"]);
}

/// The heredoc-address invariant across an arithmetic boundary: one
/// statement carrying heredoc A, then a `$(( ))` holding a `$(...)` with
/// heredoc B, then heredoc C. The plan must publish flat indices 0/1/2, and
/// `expand_fragment` must resolve each index to the SAME body the plan
/// published — an address that resolves to a different body than the one it
/// published is the worst failure this surface can have.
#[test]
fn a_heredoc_reached_through_arithmetic_keeps_the_flat_address_invariant() {
    let source = "cat <<'A' && echo $((1 + $(cat <<'B'\nbodyB\nB\n))) && cat <<'C'\nbodyA\nA\nbodyC\nC";
    let plans = plan_program(source).expect("parses");
    let heredocs: Vec<kaish_types::plan::PlannedHeredoc> = plans[0]
        .plan
        .commands
        .iter()
        .flat_map(|c| c.heredocs.clone())
        .collect();
    assert_eq!(
        heredocs.iter().map(|h| h.index).collect::<Vec<_>>(),
        vec![0, 1, 2],
        "flat indices across the arithmetic boundary"
    );
    assert_eq!(
        heredocs.iter().map(|h| h.body.display()).collect::<Vec<_>>(),
        vec!["bodyA\n".to_string(), "bodyB\n".to_string(), "bodyC\n".to_string()]
    );

    for heredoc in &heredocs {
        let addr = kaish_types::plan::FragmentAddr::new(0, heredoc.index);
        let expanded = kaish_kernel::expand_fragment(source, addr, &[])
            .unwrap_or_else(|e| panic!("address {addr:?} unresolvable: {e}"));
        assert_eq!(
            expanded,
            kaish_types::plan::Expansion::Complete(heredoc.body.display()),
            "the resolver must agree with the address the plan published for index {}",
            heredoc.index
        );
    }
}
