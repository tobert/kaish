//! Run a wrapped command through the kernel, in a kernel where nothing else
//! spawns.
//!
//! Groups 5–11 of `docs/wrapped_command.md`, "Testing, by exposure": the
//! hermetic environment, the pinned executable, the stdin contract, the cancel
//! discipline, the policy gates, the output limits and JSON binding, and the
//! text the declaration publishes.
//!
//! Every kernel here is built with `allow_external_commands = false`
//! (`KernelConfig::isolated()`), so a wrapper that runs proves it runs *as a
//! registered tool* and not through the external-command path.
//!
//! The fixtures are real host programs (`/bin/true`, `/bin/echo`, `/bin/cat`,
//! `/bin/sleep`, `/usr/bin/env`) so nothing has to be built first. `/bin/echo`
//! doubles as the argv oracle: it prints the argv the wrapper rendered.
#![cfg(all(feature = "subprocess", feature = "localfs"))]
// Test-fixture code: a panic on known-good setup IS the test failing.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;
use std::sync::Arc;
use std::time::{Duration, Instant};

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::tools::wrapped::{
    Flag, Positional, Stdin, Tail, Verb, WrappedCommand, WrappedTool,
};
use kaish_kernel::vfs::{LocalFs, VfsRouter};
use kaish_kernel::{ExecuteOptions, Kernel, KernelBackend, KernelConfig, LocalBackend};

// ── Fixtures ───────────────────────────────────────────────────────────

/// The in-tree git-shaped declaration, over `/bin/echo` so it runs anywhere.
///
/// The child prints the argv the wrapper rendered, which makes every
/// execution assertion in this file an assertion about the real child's
/// argv — not about a plan the kernel then had to carry out correctly.
fn git() -> WrappedTool {
    WrappedCommand::new("wgit")
        .executable("/bin/echo")
        .about("Version control, read-mostly.")
        .lead(["--no-pager"])
        .env("GIT_PAGER", "cat")
        .verb(
            Verb::new("status")
                .about("Show the working tree status")
                .flag(Flag::switch("short").alias("-s"))
                .flag(Flag::value("untracked-files").choices(["no", "normal", "all"]))
                .positional(Positional::many("pathspec")),
        )
        .verb(
            Verb::new("log")
                .about("Show commit history")
                .flag(Flag::value("max-count").alias("-n").int())
                .flag(Flag::switch("oneline"))
                .positional(Positional::many("revision")),
        )
        .verb(
            Verb::new("commit")
                .flag(Flag::value("message").alias("-m").repeatable().required())
                .positional(Positional::many("pathspec")),
        )
        .verb(Verb::new("passthrough").tail(Tail::Forward))
        .build()
        .expect("the git fixture declaration builds")
}

/// A root-verb wrapper over one host program, with the given stdin posture.
fn program(name: &str, executable: &str, stdin: Stdin) -> WrappedTool {
    WrappedCommand::new(name)
        .executable(executable)
        .root(
            Verb::root()
                .stdin(stdin)
                .positional(Positional::many("args"))
                .tail(Tail::Forward),
        )
        .build()
        .unwrap_or_else(|e| panic!("{name} over {executable} should build: {e}"))
}

/// A verb whose whole argv is fixed by its `lead`, so `/bin/echo` prints
/// exactly `text` and nothing else.
fn echoing_verb(name: &str, text: &str) -> Verb {
    Verb::new(name).lead([text]).omit_name()
}

fn kernel_with(dir: &Path, tools: Vec<WrappedTool>) -> Arc<Kernel> {
    kernel_with_config(dir, tools, KernelConfig::isolated())
}

fn kernel_with_config(dir: &Path, tools: Vec<WrappedTool>, config: KernelConfig) -> Arc<Kernel> {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", LocalFs::new(dir));
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, config, |_| {}, move |registry| {
        for tool in tools {
            registry.register(tool);
        }
    })
    .expect("with_backend kernel")
    .into_arc()
}

async fn run(kernel: &Kernel, script: &str) -> ExecResult {
    kernel.execute(script).await.expect("kernel execute")
}

/// The result, or the message the validator refused with before anything ran.
///
/// A wrapped command's refusals are reachable from both sides: the validator
/// judges a literal call and `Kernel::execute` returns `Err` without running
/// it, while a call whose value only exists at runtime reaches `execute` and
/// comes back as an exit-2 `ExecResult`.
async fn attempt(kernel: &Kernel, script: &str) -> Result<ExecResult, String> {
    kernel.execute(script).await.map_err(|e| e.to_string())
}

/// `Some(path)` when the host has an executable there, else `None` — for the
/// one fixture (`python3`) that is not on every host.
fn optional_program(path: &str) -> Option<&str> {
    Path::new(path).is_file().then_some(path)
}

// ── 5. Environment ─────────────────────────────────────────────────────

/// `/usr/bin/env` prints the child's whole environment, so it is the oracle
/// for every claim in this group.
fn env_tool(name: &str) -> WrappedCommand {
    WrappedCommand::new(name)
        .executable("/usr/bin/env")
        .root(Verb::root())
}

#[tokio::test]
async fn the_kaish_processs_os_environment_does_not_reach_the_child() {
    // The canary is a variable this process really has, read at runtime: no
    // `set_var`, so nothing races the other tests cargo runs in parallel.
    let host_path = std::env::var("PATH").expect("the test process has a PATH");
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![env_tool("wenv").build().unwrap()]);

    let result = run(&kernel, "wenv").await;
    assert_eq!(result.code, 0, "wenv failed: {}", result.err);
    assert!(
        !result.text_out().contains(&format!("PATH={host_path}")),
        "the child inherited the kaish process's PATH: {}",
        result.text_out()
    );
}

#[tokio::test]
async fn an_exported_scope_variable_reaches_the_child() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![env_tool("wenv").build().unwrap()]);

    let result = run(&kernel, "export SHOWN=yes\nwenv").await;
    assert_eq!(result.code, 0, "wenv failed: {}", result.err);
    assert!(
        result.text_out().contains("SHOWN=yes"),
        "an exported variable must reach the child: {}",
        result.text_out()
    );
}

#[tokio::test]
async fn an_unexported_scope_variable_does_not_reach_the_child() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![env_tool("wenv").build().unwrap()]);

    let result = run(&kernel, "HIDDEN=no\nwenv").await;
    assert_eq!(result.code, 0, "wenv failed: {}", result.err);
    assert!(
        !result.text_out().contains("HIDDEN="),
        "an unexported variable must not cross the process boundary: {}",
        result.text_out()
    );
}

#[tokio::test]
async fn a_pin_overrides_an_exported_variable_of_the_same_name() {
    let dir = tempfile::tempdir().unwrap();
    let tool = env_tool("wenv")
        .env("PINNED", "from-pin")
        .build()
        .unwrap();
    let kernel = kernel_with(dir.path(), vec![tool]);

    let result = run(&kernel, "export PINNED=from-scope\nwenv").await;
    assert_eq!(result.code, 0, "wenv failed: {}", result.err);
    assert!(
        result.text_out().contains("PINNED=from-pin"),
        "the declaration's pin must win: {}",
        result.text_out()
    );
    assert!(
        !result.text_out().contains("from-scope"),
        "the scope value must not survive alongside the pin: {}",
        result.text_out()
    );
}

#[tokio::test]
async fn a_structured_exported_value_fails_rather_than_crossing_as_json() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![env_tool("wenv").build().unwrap()]);

    let result = run(&kernel, "STRUCTURED=[a b]\nexport STRUCTURED\nwenv").await;
    assert_ne!(
        result.code, 0,
        "a list must not be stringified into the child's environment: {}",
        result.text_out()
    );
    assert!(
        result.err.contains("STRUCTURED"),
        "the refusal must name the variable: {}",
        result.err
    );
}

// ── 6. The pinned executable ───────────────────────────────────────────

#[tokio::test]
async fn a_script_that_empties_path_still_runs_the_wrapper() {
    // The whole point of pinning: `$PATH` is not consulted, so a script that
    // destroys it changes nothing about which program runs.
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![program("wtrue", "/bin/true", Stdin::Closed)]);

    let result = run(&kernel, "export PATH=/nonexistent\nwtrue").await;
    assert_eq!(
        result.code, 0,
        "the pinned executable must run without a usable PATH: {}",
        result.err
    );
}

// ── 7. stdin ───────────────────────────────────────────────────────────

#[tokio::test]
async fn a_closed_verb_refuses_piped_input_rather_than_dropping_it() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![program("wcat", "/bin/cat", Stdin::Closed)]);

    let result = run(&kernel, "echo x | wcat").await;
    assert_eq!(result.code, 2, "expected a usage refusal: {:?}", result.err);
    assert!(
        result.err.contains("wcat: does not read stdin"),
        "the refusal must name the command and the reason: {}",
        result.err
    );
}

#[tokio::test]
async fn a_closed_verb_refuses_a_redirect_too() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("input.txt"), "x\n").unwrap();
    let kernel = kernel_with(dir.path(), vec![program("wcat", "/bin/cat", Stdin::Closed)]);

    let result = run(&kernel, "wcat < input.txt").await;
    assert_eq!(result.code, 2, "expected a usage refusal: {:?}", result.err);
    assert!(
        result.err.contains("does not read stdin"),
        "the refusal must name the reason: {}",
        result.err
    );
}

#[tokio::test]
async fn a_closed_verb_with_no_input_runs() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![program("wtrue", "/bin/true", Stdin::Closed)]);

    let result = run(&kernel, "wtrue").await;
    assert_eq!(result.code, 0, "no stdin is not a refusal: {}", result.err);
}

#[tokio::test]
async fn a_pipe_verb_streams_stdin_to_the_child() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![program("wcat", "/bin/cat", Stdin::Pipe)]);

    let result = run(&kernel, "echo hello | wcat").await;
    assert_eq!(result.code, 0, "wcat failed: {}", result.err);
    assert_eq!(result.text_out().trim(), "hello");
}

#[tokio::test]
async fn a_pipe_verb_reads_a_redirect_too() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("input.txt"), "from-a-file\n").unwrap();
    let kernel = kernel_with(dir.path(), vec![program("wcat", "/bin/cat", Stdin::Pipe)]);

    let result = run(&kernel, "wcat < input.txt").await;
    assert_eq!(result.code, 0, "wcat failed: {}", result.err);
    assert_eq!(result.text_out().trim(), "from-a-file");
}

#[tokio::test]
async fn a_slow_upstream_stage_does_not_deadlock_a_pipe_verb() {
    // The no-pre-drain rule: reading the pipe before the spawn would block on
    // the still-running `sleep`, and the pipeline would never finish.
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![program("wcat", "/bin/cat", Stdin::Pipe)]);

    let result = tokio::time::timeout(Duration::from_secs(20), run(&kernel, "sleep 1 | wcat"))
        .await
        .expect("`sleep 1 | wcat` must not deadlock");
    assert_eq!(result.code, 0, "wcat failed: {}", result.err);
}

#[tokio::test]
async fn a_partial_read_leaves_the_rest_for_the_child() {
    // `read` takes one line and keeps the remainder, so the wrapper's stdin is
    // a buffered prefix AND a live pipe. `StdinPolicy::Piped` carries both and
    // writes the prefix first; the child must see them in order, once each.
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![program("wcat", "/bin/cat", Stdin::Pipe)]);

    let (writer, reader) = kaish_kernel::pipe_stream_default();
    writer.write_bytes(b"one\ntwo\nthree\n").await.unwrap();
    drop(writer); // EOF

    let result = kernel
        .execute_with_pipe_stdin("read first\nwcat", ExecuteOptions::new(), reader)
        .await
        .expect("kernel execute");
    assert_eq!(result.code, 0, "wcat failed: {}", result.err);
    assert_eq!(
        result.text_out().trim(),
        "two\nthree",
        "the child must get everything after the line `read` took, and nothing twice"
    );
}

// ── 8. Cancellation ────────────────────────────────────────────────────

#[tokio::test]
async fn timeout_kills_a_wrapped_child_promptly() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![program("wsleep", "/bin/sleep", Stdin::Closed)]);

    let started = Instant::now();
    let result = run(&kernel, "timeout 1 wsleep 10").await;
    let elapsed = started.elapsed();

    assert_ne!(result.code, 0, "a killed child does not succeed");
    assert!(
        elapsed < Duration::from_secs(3),
        "the cancel cascade must not wait out the child: took {elapsed:?}"
    );
}

#[tokio::test]
async fn kill_terminates_a_background_wrapped_child() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![program("wsleep", "/bin/sleep", Stdin::Closed)]);

    let started = Instant::now();
    let result = run(&kernel, "wsleep 30 & kill %1").await;
    let elapsed = started.elapsed();

    assert_eq!(result.code, 0, "kill %1 should terminate the job: {}", result.err);
    assert!(
        elapsed < Duration::from_secs(10),
        "kill %1 must not wait out the child: took {elapsed:?}"
    );
}

// ── 9. Feature and policy gates ────────────────────────────────────────

#[tokio::test]
async fn a_wrapper_runs_in_a_kernel_where_an_external_command_does_not() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![program("wtrue", "/bin/true", Stdin::Closed)]);

    let wrapped = run(&kernel, "wtrue").await;
    assert_eq!(wrapped.code, 0, "the wrapper must run: {}", wrapped.err);

    // The same program, the same kernel, reached the ordinary way.
    let external = run(&kernel, "/bin/true").await;
    assert_ne!(
        external.code, 0,
        "allow_external_commands=false must still refuse /bin/true: {}",
        external.text_out()
    );
}

// ── 10. Output ─────────────────────────────────────────────────────────

/// A kernel whose output limit caps stdout well below what the fixture emits.
fn small_limit() -> KernelConfig {
    let mut limit = kaish_kernel::output_limit::OutputLimitConfig::agent();
    limit.set_limit(Some(2_048));
    KernelConfig::isolated().with_output_limit(limit)
}

#[tokio::test]
async fn oversize_stdout_is_capped_the_same_way_an_external_commands_is() {
    let dir = tempfile::tempdir().unwrap();
    let big = "x".repeat(200_000);
    std::fs::write(dir.path().join("big.txt"), &big).unwrap();

    // The wrapper.
    let wrapped_kernel = kernel_with_config(
        dir.path(),
        vec![program("wcat", "/bin/cat", Stdin::Closed)],
        small_limit(),
    );
    let wrapped = run(&wrapped_kernel, "wcat big.txt").await;

    // The same program as an external command, under the same limit.
    let external_kernel = kernel_with_config(
        dir.path(),
        Vec::new(),
        small_limit().with_allow_external_commands(true),
    );
    let external = run(&external_kernel, "/bin/cat big.txt").await;

    assert_eq!(external.code, 3, "control: an external command spills: {}", external.err);
    assert!(external.did_spill, "control: an external command sets did_spill");
    assert_eq!(
        wrapped.code, external.code,
        "a wrapped command must be capped like an external one: {}",
        wrapped.err
    );
    assert_eq!(
        wrapped.did_spill, external.did_spill,
        "a wrapped command must report the spill like an external one"
    );
    assert!(
        wrapped.text_out().len() < big.len(),
        "the capped output must be smaller than what the child wrote"
    );
}

/// A tool whose every verb is JSON, so the ROOT schema claims
/// `typed_substitution` as well.
fn all_json_tool() -> WrappedTool {
    WrappedCommand::new("wjson")
        .executable("/bin/echo")
        .verb(echoing_verb("read", r#"{"k":"v"}"#).json_output())
        .build()
        .expect("the all-JSON declaration builds")
}

/// A tool with one JSON verb among text ones — the shape whose root schema
/// must NOT claim `typed_substitution`.
fn mixed_tool() -> WrappedTool {
    WrappedCommand::new("wmixed")
        .executable("/bin/echo")
        .verb(echoing_verb("text", "plain words"))
        .verb(echoing_verb("data", r#"{"k":"v"}"#).json_output())
        .verb(echoing_verb("broken", "not json at all").json_output())
        .build()
        .expect("the mixed declaration builds")
}

#[tokio::test]
async fn a_json_verb_whose_stdout_overflows_the_capture_ring_passes_through_unparsed() {
    // The capture ring is a fixed 10 MB; past it the JSON is known to be
    // incomplete, so the wrapper must hand the spilled result through
    // untouched rather than fail it as "does not parse". 11 MB of `x` is
    // not JSON at any length, so a missing guard would show as exit 1.
    let dir = tempfile::tempdir().unwrap();
    let big = "x".repeat(11_000_000);
    std::fs::write(dir.path().join("big.txt"), &big).unwrap();
    let tool = WrappedCommand::new("wjsoncat")
        .executable("/bin/cat")
        .root(Verb::root().positional(Positional::one("file")).json_output())
        .build()
        .expect("the JSON cat declaration builds");
    let kernel = kernel_with(dir.path(), vec![tool]);

    let result = run(&kernel, "wjsoncat big.txt").await;
    assert!(result.did_spill, "11 MB must overflow the 10 MB ring: code={} err={}", result.code, result.err);
    assert!(
        !result.err.contains("declared JSON output"),
        "a spilled result must not be parsed as JSON: {}",
        result.err
    );
    assert_ne!(result.code, 1, "the child's outcome passes through, not the wrapper's parse failure");
}

#[tokio::test]
async fn a_json_verb_prints_its_json_and_binds_typed_on_an_all_json_tool() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![all_json_tool()]);

    // A data-only result would print nothing; the child's own stdout is kept.
    let printed = run(&kernel, "wjson read").await;
    assert_eq!(printed.code, 0, "wjson read failed: {}", printed.err);
    assert_eq!(printed.text_out().trim(), r#"{"k":"v"}"#);

    let typed = run(&kernel, "x=$(wjson read)\ntypeof $x").await;
    assert_eq!(typed.text_out().trim(), "record", "err={}", typed.err);

    let indexed = run(&kernel, "x=$(wjson read)\necho ${x[k]}").await;
    assert_eq!(indexed.text_out().trim(), "v", "err={}", indexed.err);
}

#[tokio::test]
async fn a_json_verb_on_a_mixed_tool_binds_typed_too() {
    // The root schema does not claim `typed_substitution` here — the verb
    // stamps `data_is_value` on its own result, and the kernel ORs it in.
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![mixed_tool()]);

    let typed = run(&kernel, "x=$(wmixed data)\ntypeof $x").await;
    assert_eq!(typed.text_out().trim(), "record", "err={}", typed.err);

    let indexed = run(&kernel, "x=$(wmixed data)\necho ${x[k]}").await;
    assert_eq!(indexed.text_out().trim(), "v", "err={}", indexed.err);
}

#[tokio::test]
async fn a_text_verbs_substitution_stays_text() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![mixed_tool()]);

    let typed = run(&kernel, "x=$(wmixed text)\ntypeof $x").await;
    assert_eq!(typed.text_out().trim(), "string", "err={}", typed.err);
}

#[tokio::test]
async fn stdout_that_is_not_json_from_a_json_verb_is_an_error() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![mixed_tool()]);

    let result = run(&kernel, "wmixed broken").await;
    assert_eq!(result.code, 1, "a broken promise is an error: {:?}", result.err);
    assert!(
        result.err.contains("wmixed broken"),
        "the error must name the verb: {}",
        result.err
    );
    assert!(
        result.err.contains("JSON"),
        "the error must say what was expected: {}",
        result.err
    );
}

#[tokio::test]
async fn a_json_verb_over_python_json_tool_binds_a_real_documents_shape() {
    let Some(python) = optional_program("/usr/bin/python3") else {
        println!("skipped: /usr/bin/python3 is not installed on this host");
        return;
    };
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("doc.json"), r#"{"name":"kaish","verbs":3}"#).unwrap();

    let tool = WrappedCommand::new("wpy")
        .executable(python)
        .lead(["-I"])
        .verb(
            Verb::new("json-tool")
                .lead(["-m", "json.tool"])
                .omit_name()
                .positional(Positional::one("file"))
                .json_output(),
        )
        .build()
        .expect("the python declaration builds");
    let kernel = kernel_with(dir.path(), vec![tool]);

    let result = run(&kernel, "x=$(wpy json-tool doc.json)\necho ${x[name]}").await;
    assert_eq!(result.text_out().trim(), "kaish", "err={}", result.err);
}

// ── 11. Published text ─────────────────────────────────────────────────

#[tokio::test]
async fn help_renders_the_verbs_their_flags_and_their_constraints() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![git()]);

    let result = run(&kernel, "help wgit").await;
    let text = result.text_out().into_owned();

    for verb in ["status", "log", "commit", "passthrough"] {
        assert!(text.contains(verb), "help must list the verb '{verb}':\n{text}");
    }
    assert!(
        text.contains("max-count"),
        "help must list a verb's flags:\n{text}"
    );
    assert!(
        text.contains("(also: -n)"),
        "help must name the alias an agent would write:\n{text}"
    );
    assert!(
        text.contains("one of: no, normal, all"),
        "help must carry the constraint in the description:\n{text}"
    );
    assert!(
        text.contains("forwards undeclared flags"),
        "help must mark the verb that pays the override's cost:\n{text}"
    );
}

#[tokio::test]
async fn the_validator_reports_an_unknown_flag_before_execution() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![git()]);

    let result = run(&kernel, "kaish-validate -e 'wgit log --output=/tmp/x'").await;
    assert_ne!(result.code, 0, "an unknown flag is a validation error");
    let text = format!("{}{}", result.text_out(), result.err);
    assert!(
        text.contains("unknown flag '--output' for 'wgit log'"),
        "validation must name the flag and the scope:\n{text}"
    );
    assert!(
        text.contains("--oneline"),
        "validation must name the allowed set:\n{text}"
    );
}

#[tokio::test]
async fn a_computed_word_in_flag_position_is_not_judged_at_validation() {
    // The validator has no value for `$x`, so it has no verdict. Execution
    // parses the expanded text like a literal and refuses it there.
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![git()]);

    let validated = run(
        &kernel,
        "kaish-validate -e 'x=\"--output=/tmp/x\"\nwgit log $x'",
    )
    .await;
    let text = format!("{}{}", validated.text_out(), validated.err);
    assert!(
        !text.contains("unknown flag"),
        "an opaque word must not be reported as an unknown flag:\n{text}"
    );

    let executed = run(&kernel, "x=\"--output=/tmp/x\"\nwgit log $x").await;
    assert_eq!(
        executed.code, 2,
        "execution refuses the expanded value: {:?}",
        executed.err
    );
    assert!(
        executed.err.contains("unknown flag '--output' for 'wgit log'"),
        "the execution refusal names the flag: {}",
        executed.err
    );
    assert!(
        executed.text_out().is_empty(),
        "nothing spawned, so nothing printed: {}",
        executed.text_out()
    );
}

// ── The rendered argv reaches the child ────────────────────────────────

#[tokio::test]
async fn the_child_receives_the_argv_the_wrapper_rendered() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![git()]);

    let result = run(&kernel, "wgit log --oneline -n 5").await;
    assert_eq!(result.code, 0, "wgit log failed: {}", result.err);
    assert_eq!(
        result.text_out().trim(),
        "--no-pager log --oneline --max-count=5",
        "the child must see the canonical spelling, in source order"
    );
}

#[tokio::test]
async fn an_undeclared_flag_and_its_value_stay_together_under_forward() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![git()]);

    let result = run(&kernel, "wgit passthrough --message-format json").await;
    assert_eq!(result.code, 0, "wgit passthrough failed: {}", result.err);
    assert_eq!(
        result.text_out().trim(),
        "--no-pager passthrough --message-format json"
    );
}

#[tokio::test]
async fn an_unknown_verb_never_reaches_execution() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_with(dir.path(), vec![git()]);

    let refusal = attempt(&kernel, "wgit comit -m x")
        .await
        .expect_err("a literal unknown verb is refused before the script runs");
    assert!(
        refusal.contains("unknown verb 'comit'"),
        "the refusal names the word: {refusal}"
    );
    assert!(
        refusal.contains("commit, log, passthrough, status"),
        "the refusal names the allowed set: {refusal}"
    );
}

/// A `path_under` positional's relative value is resolved against the real
/// cwd at execution, and the child opens the canonical path.
#[tokio::test]
async fn a_relative_path_under_value_reaches_the_child_canonical() {
    let dir = tempfile::tempdir().unwrap();
    let real = std::fs::canonicalize(dir.path()).unwrap();
    let scripts = real.join("scripts");
    std::fs::create_dir(&scripts).unwrap();
    std::fs::write(scripts.join("etl.py"), "").unwrap();

    let tool = WrappedCommand::new("wrun")
        .executable("/bin/echo")
        .root(Verb::root().positional(Positional::one("script").required().path_under(&scripts)))
        .build()
        .expect("the path_under declaration builds");
    let kernel = kernel_with(dir.path(), vec![tool]);

    let ok = run(&kernel, "cd scripts\nwrun etl.py").await;
    assert_eq!(ok.code, 0, "wrun failed: {}", ok.err);
    assert_eq!(
        ok.text_out().trim(),
        scripts.join("etl.py").display().to_string(),
        "the child must open the path kaish checked, not the word the agent wrote"
    );

    // An absolute value is decided when the call is planned, so the validator
    // refuses it and the script never runs.
    let refused = attempt(&kernel, "wrun /etc/passwd")
        .await
        .expect_err("an absolute path outside the root is decided before execution");
    assert!(
        refused.contains("must be under"),
        "the refusal names the root: {refused}"
    );
    assert!(
        refused.contains("Got '/etc/passwd'."),
        "the refusal names the value: {refused}"
    );
}

// ── Nested verbs ────────────────────────────────────────────────────────

#[tokio::test]
async fn a_nested_verb_spawns_with_the_full_path_and_leads_in_argv() {
    let dir = tempfile::tempdir().unwrap();
    let tool = WrappedCommand::new("wgit2")
        .executable("/bin/echo")
        .lead(["--no-pager"])
        .verb(Verb::new("worktree").verb(Verb::new("list").flag(Flag::switch("porcelain"))))
        .build()
        .expect("the nested declaration builds");
    let kernel = kernel_with(dir.path(), vec![tool]);

    let result = run(&kernel, "wgit2 worktree list --porcelain").await;
    assert_eq!(result.code, 0, "wgit2 worktree list failed: {}", result.err);
    assert_eq!(
        result.text_out().trim(),
        "--no-pager worktree list --porcelain",
        "the full path, with every level's lead, must reach the child"
    );
}

#[tokio::test]
async fn a_bare_node_refuses_before_anything_spawns() {
    let dir = tempfile::tempdir().unwrap();
    let tool = WrappedCommand::new("wgit3")
        .executable("/bin/echo")
        .verb(
            Verb::new("worktree")
                .verb(Verb::new("add"))
                .verb(Verb::new("list")),
        )
        .build()
        .expect("the nested declaration builds");
    let kernel = kernel_with(dir.path(), vec![tool]);

    // A literal bare node is caught at validation, before the script runs —
    // the same lane an unknown verb is refused from.
    let refusal = attempt(&kernel, "wgit3 worktree")
        .await
        .expect_err("a bare node cannot run and never spawns");
    assert!(refusal.contains("needs a verb"), "{refusal}");
    assert!(refusal.contains("add, list"), "{refusal}");
}
