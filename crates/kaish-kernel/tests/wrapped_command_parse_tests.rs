//! Parse and render a wrapped command's argv, and refuse everything the
//! declaration does not describe.
//!
//! Groups 1 and 2 of `docs/wrapped_command.md`, "Testing, by exposure": every
//! flag form, `--` placement, each `Tail` mode, the constraint vocabulary, the
//! spellings kaish deliberately does not accept, and the injection corpus. The
//! three declarations the design doc shows are fixtures here, and the argv
//! they render is asserted word for word.
//!
//! Wrapped commands run external programs, so this binary is empty without
//! the `subprocess` feature (it is on under `cargo test --all` through
//! workspace feature unification).
#![cfg(feature = "subprocess")]
// Test-fixture code: a panic on known-good setup IS the test failing.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::{Path, PathBuf};

use rstest::rstest;

use kaish_kernel::tools::wrapped::{
    Flag, Positional, Stdin, Tail, Verb, WrappedCommand, WrappedTool,
};
use kaish_kernel::tools::ToolArgs;
use kaish_kernel::ast::Value;

/// A real, executable file so `build()`'s verification has something to
/// accept. The declarations below pin this instead of `/usr/bin/git` so the
/// suite does not depend on what the host has installed; the rendered argv
/// never contains the executable, so every assertion is unaffected.
fn executable(dir: &Path) -> PathBuf {
    let path = dir.join("program");
    std::fs::write(&path, "#!/bin/sh\nexit 0\n").unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    }
    path
}

/// A built tool, with the tempdir holding its executable alive alongside it.
struct Fixture {
    _dir: tempfile::TempDir,
    tool: WrappedTool,
}

impl Fixture {
    /// The rendered argv, or the message the agent would see.
    fn plan(&self, words: &[&str]) -> Result<Vec<String>, String> {
        let mut args = ToolArgs::new();
        for word in words {
            args.positional.push(Value::String((*word).to_string()));
        }
        self.tool
            .plan_call(&args)
            .map(|call| call.argv)
            .map_err(|error| error.to_string())
    }

    fn plan_call(&self, words: &[&str]) -> kaish_kernel::tools::wrapped::RenderedCall {
        let mut args = ToolArgs::new();
        for word in words {
            args.positional.push(Value::String((*word).to_string()));
        }
        self.tool.plan_call(&args).expect("the call should plan")
    }

    fn refuse(&self, words: &[&str]) -> String {
        self.plan(words).expect_err("the call should be refused")
    }

    fn argv(&self, words: &[&str]) -> Vec<String> {
        self.plan(words).expect("the call should plan")
    }
}

fn build(declaration: impl FnOnce(PathBuf) -> WrappedCommand) -> Fixture {
    let dir = tempfile::tempdir().unwrap();
    let path = executable(dir.path());
    let tool = declaration(path).build().expect("the declaration should build");
    Fixture { _dir: dir, tool }
}

// ── The three declarations the design doc shows ────────────────────────

/// `git`, allowlisted verbs. Word for word the declaration in
/// `docs/wrapped_command.md`, "git: allowlisted verbs".
fn git() -> Fixture {
    build(|path| {
        WrappedCommand::new("git")
            .executable(path)
            .about("Version control, read-mostly. Push needs an explicit remote and refspec.")
            .lead(["--no-pager"])
            .env("GIT_PAGER", "cat")
            .env("GIT_TERMINAL_PROMPT", "0")
            .verb(
                Verb::new("status")
                    .flag(Flag::switch("short").alias("-s"))
                    .flag(Flag::value("untracked-files").choices(["no", "normal", "all"]))
                    .positional(Positional::many("pathspec")),
            )
            .verb(
                Verb::new("log")
                    .flag(Flag::value("max-count").alias("-n").int())
                    .flag(Flag::switch("oneline"))
                    .flag(Flag::value("since"))
                    .positional(Positional::many("revision")),
            )
            .verb(
                Verb::new("diff")
                    .flag(Flag::switch("stat"))
                    .flag(Flag::switch("cached"))
                    .positional(Positional::many("pathspec")),
            )
            .verb(
                Verb::new("commit")
                    .flag(Flag::value("message").alias("-m").repeatable().required())
                    .positional(Positional::many("pathspec")),
            )
            .verb(
                Verb::new("push")
                    .flag(Flag::switch("force-with-lease"))
                    .positional(Positional::one("remote").required())
                    .positional(Positional::one("refspec").required()),
            )
    })
}

/// `python`, locked: run the scripts this deployment ships. The `path_under`
/// root is the tempdir so the check has a real directory to resolve against.
fn python_locked() -> Fixture {
    let dir = tempfile::tempdir().unwrap();
    let path = executable(dir.path());
    let scripts = dir.path().join("scripts");
    std::fs::create_dir(&scripts).unwrap();
    std::fs::write(scripts.join("etl.py"), "").unwrap();
    let tool = WrappedCommand::new("python")
        .executable(path)
        .lead(["-I"])
        .env("PYTHONDONTWRITEBYTECODE", "1")
        .root(
            Verb::root()
                .positional(Positional::one("script").required().path_under(&scripts))
                .positional(Positional::many("args"))
                .tail(Tail::AfterDashDash)
                .stdin(Stdin::Pipe),
        )
        .build()
        .expect("the declaration should build");
    Fixture { _dir: dir, tool }
}

/// `python`, interpreter posture: deliberately open, with the module list as
/// the allowlist. `json-tool` carries `omit_name()` — the doc's declaration
/// leaves it off, which would render the synthetic verb name into the child's
/// argv.
fn python_open() -> Fixture {
    build(|path| {
        WrappedCommand::new("python")
            .executable(path)
            .lead(["-I"])
            .root(
                Verb::root()
                    .flag(Flag::value("m").choices(["json.tool", "pytest", "http.server", "venv"]))
                    .flag(Flag::value("c").about("Run inline code. This deployment allows it."))
                    .positional(Positional::one("script"))
                    .positional(Positional::many("args"))
                    .stdin(Stdin::Pipe),
            )
            .verb(
                Verb::new("json-tool")
                    .lead(["-m", "json.tool"])
                    .omit_name()
                    .positional(Positional::one("file"))
                    .stdin(Stdin::Pipe)
                    .json_output(),
            )
    })
}

/// `cargo`: the override, scoped to a verb.
fn cargo() -> Fixture {
    build(|path| {
        WrappedCommand::new("cargo")
            .executable(path)
            .env("CARGO_TERM_COLOR", "never")
            .env("CARGO_NET_OFFLINE", "true")
            .verb(
                Verb::new("build")
                    .flag(Flag::switch("release"))
                    .flag(Flag::value("package").alias("-p")),
            )
            .verb(
                Verb::new("test")
                    .flag(Flag::value("package").alias("-p"))
                    .positional(Positional::one("testname"))
                    .tail(Tail::AfterDashDash),
            )
            .verb(Verb::new("clippy").tail(Tail::Forward))
            .verb(
                Verb::new("metadata")
                    .lead(["--format-version", "1"])
                    .json_output(),
            )
    })
}

// ── 1. The argv the design doc shows, word for word ────────────────────

#[test]
fn git_log_renders_the_argv_the_design_doc_shows() {
    // `git log -n 5 -- "$branch"` with branch="--output=/etc/cron.d/x".
    // The executable is argv[0] at the spawn site, so it is not rendered here.
    assert_eq!(
        git().argv(&["log", "-n", "5", "--", "--output=/etc/cron.d/x"]),
        vec!["--no-pager", "log", "--max-count=5", "--", "--output=/etc/cron.d/x"]
    );
}

#[test]
fn cargo_test_renders_the_argv_the_design_doc_shows() {
    assert_eq!(
        cargo().argv(&["test", "parser", "--", "--nocapture"]),
        vec!["test", "parser", "--", "--nocapture"]
    );
}

#[test]
fn cargo_clippy_renders_the_argv_the_design_doc_shows() {
    assert_eq!(
        cargo().argv(&["clippy", "--all-targets", "--", "-D", "warnings"]),
        vec!["clippy", "--all-targets", "--", "-D", "warnings"]
    );
}

#[test]
fn python_locked_renders_the_resolved_script_path() {
    // `python etl.py -- --stage=1` — the design doc's argv, with the pinned
    // scripts directory standing in for /opt/app/scripts.
    let fixture = python_locked();
    let planned = fixture.plan_call(&["etl.py", "--", "--stage=1"]);
    assert_eq!(planned.stdin, Stdin::Pipe);
    assert_eq!(planned.argv, vec!["-I", "etl.py", "--", "--stage=1"]);
    // The value is relative, so the resolved path is the execute lane's to
    // substitute; the check names where.
    assert_eq!(planned.path_checks.len(), 1);
    assert_eq!(planned.path_checks[0].positional, "script");
    assert_eq!(planned.path_checks[0].argv_index, 1);
    assert!(!planned.path_checks[0].resolved);
}

#[test]
fn the_design_docs_refusals_read_word_for_word() {
    let git = git();
    assert_eq!(
        git.refuse(&["comit", "-m", "x"]),
        "git: unknown verb 'comit'. Allowed: commit, diff, log, push, status"
    );
    assert_eq!(
        git.refuse(&["log", "--output=/tmp/x"]),
        "git: unknown flag '--output' for 'git log'. Allowed: -n/--max-count, --oneline, --since"
    );
    assert_eq!(
        git.refuse(&["push", "--force", "origin", "main"]),
        "git: unknown flag '--force' for 'git push'. Allowed: --force-with-lease"
    );
    assert_eq!(
        python_open().refuse(&["-x", "code"]),
        "python: unknown flag '-x' for 'python'. Allowed: -c, -m"
    );
}

#[test]
fn a_verb_python_does_not_have_renders_its_lead_instead_of_its_name() {
    assert_eq!(
        python_open().argv(&["json-tool", "f.json"]),
        vec!["-I", "-m", "json.tool", "f.json"]
    );
}

#[test]
fn a_declaration_with_a_root_and_named_verbs_falls_through_to_the_root() {
    assert_eq!(
        python_open().argv(&["etl.py", "a", "b"]),
        vec!["-I", "etl.py", "a", "b"]
    );
}

#[test]
fn a_verb_lead_renders_after_the_verb_name_when_the_name_is_kept() {
    assert_eq!(
        cargo().argv(&["metadata"]),
        vec!["metadata", "--format-version", "1"]
    );
}

// ── 2. Every flag form ─────────────────────────────────────────────────

#[rstest]
#[case::long_switch(vec!["log", "--oneline"], vec!["--no-pager", "log", "--oneline"])]
#[case::short_alias_switch(vec!["status", "-s"], vec!["--no-pager", "status", "--short"])]
#[case::long_value_separate(vec!["log", "--since", "2d"], vec!["--no-pager", "log", "--since=2d"])]
#[case::long_value_equals(vec!["log", "--since=2d"], vec!["--no-pager", "log", "--since=2d"])]
#[case::short_alias_value(vec!["log", "-n", "5"], vec!["--no-pager", "log", "--max-count=5"])]
#[case::long_name_for_short_alias(
    vec!["log", "--max-count", "5"],
    vec!["--no-pager", "log", "--max-count=5"]
)]
#[case::equals_empty_value(vec!["log", "--since="], vec!["--no-pager", "log", "--since="])]
#[case::flags_render_in_source_order(
    vec!["log", "--oneline", "-n", "5"],
    vec!["--no-pager", "log", "--oneline", "--max-count=5"]
)]
#[case::a_declared_flag_renders_after_a_positional_the_agent_wrote_first(
    vec!["log", "main", "--oneline"],
    vec!["--no-pager", "log", "main", "--oneline"]
)]
#[case::value_flag_takes_a_flag_looking_word(
    vec!["commit", "-m", "-foo"],
    vec!["--no-pager", "commit", "--message=-foo"]
)]
#[case::value_flag_takes_a_dash_dash(
    vec!["commit", "-m", "--"],
    vec!["--no-pager", "commit", "--message=--"]
)]
fn every_flag_form_renders_under_its_declared_name(
    #[case] words: Vec<&str>,
    #[case] expected: Vec<&str>,
) {
    assert_eq!(git().argv(&words), expected);
}

#[test]
fn a_one_character_name_renders_separated_not_glued() {
    assert_eq!(
        python_open().argv(&["-m", "pytest"]),
        vec!["-I", "-m", "pytest"]
    );
}

#[test]
fn a_repeatable_flag_renders_once_per_occurrence_in_source_order() {
    assert_eq!(
        git().argv(&["commit", "-m", "first", "--message", "second"]),
        vec!["--no-pager", "commit", "--message=first", "--message=second"]
    );
}

#[test]
fn a_flag_the_declaration_did_not_mark_repeatable_is_refused_twice_over() {
    assert_eq!(
        git().refuse(&["log", "--since", "2d", "--since", "3d"]),
        "git: '--since' given more than once for 'git log'."
    );
}

#[test]
fn a_switch_written_with_a_value_is_refused() {
    assert_eq!(
        git().refuse(&["log", "--oneline=1"]),
        "git: '--oneline' takes no value for 'git log'."
    );
}

#[test]
fn a_value_flag_at_the_end_of_argv_is_refused() {
    assert_eq!(
        git().refuse(&["log", "--since"]),
        "git: '--since' needs a value for 'git log'."
    );
}

// ── 2. The spellings kaish does not accept ─────────────────────────────

#[test]
fn clustered_shorts_are_refused_and_the_error_names_the_separated_form() {
    let fixture = build(|path| {
        WrappedCommand::new("probe").executable(path).verb(
            Verb::new("run")
                .flag(Flag::switch("s"))
                .flag(Flag::switch("v")),
        )
    });
    assert_eq!(
        fixture.refuse(&["run", "-sv"]),
        "probe: '-sv' is not a flag for 'probe run'. Use -s -v."
    );
}

#[test]
fn a_glued_short_value_is_refused_and_the_error_names_the_separated_form() {
    assert_eq!(
        git().refuse(&["log", "-n5"]),
        "git: '-n5' is not a flag for 'git log'. Use -n 5."
    );
}

#[test]
fn a_glued_short_value_written_with_an_equals_is_refused_too() {
    assert_eq!(
        git().refuse(&["log", "-n=5"]),
        "git: '-n=5' is not a flag for 'git log'. Use -n =5."
    );
}

#[rstest]
#[case::flag_prefix(vec!["push", "--forc"], "unknown flag '--forc'")]
#[case::flag_prefix_of_a_long_name(vec!["log", "--onel"], "unknown flag '--onel'")]
#[case::single_dash_long_name(vec!["log", "-oneline"], "unknown flag '-oneline'")]
#[case::double_dash_short_alias(vec!["log", "--n", "5"], "unknown flag '--n'")]
fn no_prefix_matching_and_no_dash_juggling(#[case] words: Vec<&str>, #[case] expected: &str) {
    let message = git().refuse(&words);
    assert!(message.contains(expected), "{message}");
}

#[rstest]
#[case::verb_prefix(vec!["lo"], "unknown verb 'lo'")]
#[case::verb_uppercase(vec!["LOG"], "unknown verb 'LOG'")]
#[case::verb_typo(vec!["comit"], "unknown verb 'comit'")]
fn verbs_match_exactly(#[case] words: Vec<&str>, #[case] expected: &str) {
    let message = git().refuse(&words);
    assert!(message.contains(expected), "{message}");
}

#[test]
fn flag_names_are_case_sensitive() {
    let message = git().refuse(&["log", "--ONELINE"]);
    assert!(message.contains("unknown flag '--ONELINE'"), "{message}");
}

#[test]
fn a_command_with_verbs_and_no_root_refuses_a_call_that_names_none() {
    assert_eq!(
        git().refuse(&[]),
        "git: no verb given. Allowed: commit, diff, log, push, status"
    );
}

// ── 3. `--` placement, and the rule that it is never inserted ──────────

#[test]
fn a_dash_dash_the_agent_wrote_stays_where_it_was() {
    // `git log -- main` means "paths named main", not "revision main".
    assert_eq!(
        git().argv(&["log", "--", "main"]),
        vec!["--no-pager", "log", "--", "main"]
    );
}

#[test]
fn the_wrapper_never_inserts_a_dash_dash_before_positionals() {
    // The hazard the previous test guards from the other side: `git log main`
    // must NOT become `git log -- main`, which would change the program's
    // meaning from "revision main" to "paths named main".
    let argv = git().argv(&["log", "main"]);
    assert_eq!(argv, vec!["--no-pager", "log", "main"]);
    assert!(
        !argv.contains(&"--".to_string()),
        "an inserted `--` rewrites what the agent asked for: {argv:?}"
    );
}

#[test]
fn a_word_after_dash_dash_is_positional_however_it_is_spelled() {
    assert_eq!(
        git().argv(&["log", "--", "--oneline"]),
        vec!["--no-pager", "log", "--", "--oneline"]
    );
}

#[test]
fn a_dash_dash_the_agent_wrote_last_also_introduces_the_tail() {
    // One `--`, not two: the agent's own already sits where the tail begins.
    let argv = cargo().argv(&["test", "parser", "--", "--nocapture"]);
    assert_eq!(argv.iter().filter(|word| *word == "--").count(), 1, "{argv:?}");
}

#[test]
fn a_dash_dash_alone_renders_when_the_agent_wrote_it() {
    assert_eq!(git().argv(&["log", "--"]), vec!["--no-pager", "log", "--"]);
}

// ── 4. Each Tail mode ──────────────────────────────────────────────────

#[test]
fn tail_deny_refuses_a_word_that_fills_no_slot() {
    assert_eq!(
        cargo().refuse(&["build", "extra"]),
        "cargo: unexpected argument 'extra'"
    );
}

#[test]
fn tail_deny_refuses_a_word_after_dash_dash_too() {
    assert_eq!(
        cargo().refuse(&["build", "--", "extra"]),
        "cargo: unexpected argument 'extra'"
    );
}

#[test]
fn tail_after_dash_dash_takes_a_word_past_the_agents_own_dash_dash() {
    assert_eq!(
        cargo().argv(&["test", "parser", "--", "extra"]),
        vec!["test", "parser", "--", "extra"]
    );
}

#[test]
fn tail_after_dash_dash_names_the_dash_dash_as_the_fix_before_one_is_written() {
    // The wrapper never inserts a `--` the agent did not write, so a word past
    // every declared slot is refused with the word that would make it legal.
    assert_eq!(
        cargo().refuse(&["test", "parser", "extra"]),
        "cargo: unexpected argument 'extra' for 'cargo test'. \
         Write -- before arguments meant for the program."
    );
}

#[test]
fn tail_after_dash_dash_still_refuses_an_undeclared_flag() {
    let message = cargo().refuse(&["test", "--nocapture"]);
    assert!(message.contains("unknown flag '--nocapture'"), "{message}");
}

#[test]
fn tail_forward_passes_an_undeclared_flag_through_in_place() {
    assert_eq!(
        cargo().argv(&["clippy", "--fix", "--all-targets"]),
        vec!["clippy", "--fix", "--all-targets"]
    );
}

#[test]
fn tail_forward_keeps_an_undeclared_flag_and_its_value_together() {
    // The case that made source order the rule: a declaration-ordered flag
    // block plus a tail rendered this as `clippy --message-format -- json`,
    // and the child saw a flag with no value.
    assert_eq!(
        cargo().argv(&["clippy", "--message-format", "json"]),
        vec!["clippy", "--message-format", "json"]
    );
}

#[test]
fn tail_forward_does_not_swallow_a_declared_verbs_own_grammar() {
    // `clippy` declares no flags, so every flag forwards. `build` declares
    // some and denies the rest — the override is scoped to the verb.
    let message = cargo().refuse(&["build", "--fix"]);
    assert!(message.contains("unknown flag '--fix'"), "{message}");
}

// ── 5. The constraint vocabulary ───────────────────────────────────────

#[test]
fn an_int_flag_refuses_a_value_that_is_not_an_integer() {
    assert_eq!(
        git().refuse(&["log", "-n", "many"]),
        "git: '--max-count' takes an integer. Got 'many'."
    );
}

#[rstest]
#[case::plain("5")]
#[case::negative("-5")]
fn an_int_flag_accepts_an_integer(#[case] value: &str) {
    assert_eq!(
        git().argv(&["log", "-n", value]),
        vec![
            "--no-pager".to_string(),
            "log".to_string(),
            format!("--max-count={value}")
        ]
    );
}

#[test]
fn a_choices_flag_refuses_a_value_outside_the_set_and_names_the_set() {
    assert_eq!(
        git().refuse(&["status", "--untracked-files=maybe"]),
        "git: '--untracked-files' must be one of: no, normal, all. Got 'maybe'."
    );
}

#[test]
fn a_choices_flag_accepts_a_value_in_the_set() {
    assert_eq!(
        git().argv(&["status", "--untracked-files=all"]),
        vec!["--no-pager", "status", "--untracked-files=all"]
    );
}

#[test]
fn a_required_flag_that_is_absent_is_refused() {
    assert_eq!(
        git().refuse(&["commit"]),
        "git: required flag '--message' not given for 'git commit'."
    );
}

#[test]
fn a_required_positional_that_is_absent_is_refused() {
    assert_eq!(
        git().refuse(&["push", "origin"]),
        "git: required argument 'refspec' not given for 'git push'."
    );
}

#[test]
fn two_required_positionals_fill_in_source_order() {
    assert_eq!(
        git().argv(&["push", "origin", "main"]),
        vec!["--no-pager", "push", "origin", "main"]
    );
}

#[test]
fn a_many_positional_absorbs_the_rest() {
    assert_eq!(
        git().argv(&["diff", "--stat", "a.rs", "b.rs", "c.rs"]),
        vec!["--no-pager", "diff", "--stat", "a.rs", "b.rs", "c.rs"]
    );
}

// ── 6. The root-verb program ───────────────────────────────────────────

#[test]
fn a_root_verb_program_never_reads_a_verb() {
    let fixture = python_locked();
    assert_eq!(fixture.argv(&["etl.py"]), vec!["-I", "etl.py"]);
}

#[test]
fn the_locked_python_posture_denies_the_three_costumes() {
    let fixture = python_locked();
    for costume in ["-c", "-m", "-"] {
        let message = fixture.refuse(&[costume, "x"]);
        assert!(
            message.starts_with(&format!("python: unknown flag '{costume}' for 'python'.")),
            "{costume}: {message}"
        );
        assert!(message.ends_with("Allowed: (none)"), "{costume}: {message}");
    }
}

#[test]
fn the_locked_python_posture_denies_an_undeclared_flag_after_the_script() {
    let fixture = python_locked();
    let message = fixture.refuse(&["etl.py", "--stage=1"]);
    assert_eq!(
        message,
        "python: unknown flag '--stage' for 'python'. Allowed: (none)"
    );
}

#[test]
fn the_open_python_posture_allows_the_modules_the_declaration_names() {
    assert_eq!(
        python_open().argv(&["-m", "json.tool"]),
        vec!["-I", "-m", "json.tool"]
    );
    assert_eq!(
        python_open().refuse(&["-m", "os"]),
        "python: '-m' must be one of: json.tool, pytest, http.server, venv. Got 'os'."
    );
}

// ── 7. Injection corpus ────────────────────────────────────────────────

#[rstest]
#[case::flag_looking_value_before_dash_dash(
    vec!["log", "--output=/etc/cron.d/x"],
    "unknown flag '--output'"
)]
#[case::bare_dash_flag(vec!["log", "-x"], "unknown flag")]
#[case::flag_looking_value_after_a_value_flag_is_data(vec!["log", "--since", "--evil"], "")]
fn a_value_that_looks_like_a_flag_is_parsed_as_one_unless_declared_otherwise(
    #[case] words: Vec<&str>,
    #[case] expected: &str,
) {
    let outcome = git().plan(&words);
    if expected.is_empty() {
        assert_eq!(
            outcome.expect("a value flag's value is data"),
            vec!["--no-pager", "log", "--since=--evil"]
        );
    } else {
        let message = outcome.expect_err("the call should be refused");
        assert!(message.contains(expected), "{message}");
    }
}

#[rstest]
#[case::dash_dash_inside_a_value("a--b")]
#[case::a_bare_dash_dash_as_data("--")]
#[case::equals_inside_a_value("KEY=VALUE")]
#[case::two_equals("a=b=c")]
#[case::empty_string("")]
#[case::unicode("ファイル.txt")]
#[case::unicode_with_combining("cafe\u{0301}.txt")]
#[case::newline_inside_a_value("a\nb")]
#[case::semicolon_and_backtick("; rm -rf /; `id`")]
#[case::dollar_substitution_text("$(id)")]
#[case::single_quote("it's")]
fn a_word_after_dash_dash_reaches_the_child_byte_for_byte(#[case] value: &str) {
    // Nothing between the parse and the child re-reads the word: there is no
    // `sh -c`, so shell metacharacters are ordinary bytes.
    assert_eq!(
        git().argv(&["log", "--", value]),
        vec!["--no-pager", "log", "--", value]
    );
}

#[rstest]
#[case::equals_inside_a_flag_value("KEY=VALUE", "--since=KEY=VALUE")]
#[case::dash_dash_inside_a_flag_value("a--b", "--since=a--b")]
#[case::empty_flag_value("", "--since=")]
#[case::unicode_flag_value("ファイル", "--since=ファイル")]
fn a_flag_value_reaches_the_child_byte_for_byte(#[case] value: &str, #[case] rendered: &str) {
    assert_eq!(
        git().argv(&["log", "--since", value]),
        vec!["--no-pager", "log", rendered]
    );
}

#[test]
fn a_nul_byte_is_refused_at_the_parse_naming_the_argument() {
    let fixture = git();
    let mut args = ToolArgs::new();
    args.positional.push(Value::String("log".to_string()));
    args.positional.push(Value::String("a\0b".to_string()));
    let error = fixture
        .tool
        .plan_call(&args)
        .expect_err("argv cannot carry a NUL byte");
    assert_eq!(
        error.to_string(),
        "git: argument 2 contains a NUL byte. argv cannot carry NUL; remove it."
    );
    assert_eq!(error.exit_code(), 2);
}

#[test]
fn every_refusal_exits_two() {
    let fixture = git();
    let mut args = ToolArgs::new();
    args.positional.push(Value::String("comit".to_string()));
    let error = fixture.tool.plan_call(&args).expect_err("unknown verb");
    assert_eq!(error.exit_code(), 2);
}

// ── 8. path_under through a planned call ───────────────────────────────

#[test]
fn a_path_constrained_positional_records_its_check() {
    let fixture = python_locked();
    let planned = fixture.plan_call(&["etl.py"]);
    assert_eq!(planned.path_checks.len(), 1);
    let check = &planned.path_checks[0];
    assert_eq!(check.positional, "script");
    assert_eq!(check.value, "etl.py");
    assert!(check.root.is_absolute());
}

#[test]
fn an_absolute_path_outside_the_root_is_refused_before_anything_spawns() {
    let fixture = python_locked();
    let message = fixture.refuse(&["/etc/passwd"]);
    assert!(
        message.starts_with("python: 'script' must be under "),
        "{message}"
    );
    assert!(message.contains("Got '/etc/passwd'."), "{message}");
}

// ── 9. The published schema ────────────────────────────────────────────

#[test]
fn the_schema_publishes_the_verbs_flags_and_constraints() {
    let fixture = git();
    let schema = fixture.tool.schema();
    assert!(schema.raw_argv);
    let names: Vec<&str> = schema.subcommands.iter().map(|s| s.name.as_str()).collect();
    assert_eq!(names, vec!["status", "log", "diff", "commit", "push"]);

    let status = &schema.subcommands[0];
    let untracked = status
        .params
        .iter()
        .find(|p| p.name == "untracked-files")
        .expect("the flag is published");
    assert_eq!(untracked.description, "one of: no, normal, all");
}

#[test]
fn a_forwarding_verb_is_marked_in_the_published_schema() {
    let fixture = cargo();
    let schema = fixture.tool.schema();
    let clippy = schema
        .subcommands
        .iter()
        .find(|s| s.name == "clippy")
        .expect("the verb is published");
    assert_eq!(clippy.description, "forwards undeclared flags");
}
