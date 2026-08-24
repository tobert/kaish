//! Unit tests for the declaration, the schema, the constraint helpers, and
//! the agreement between the two argv binders.
//!
//! The parse/render corpus that an embedder would recognize lives in
//! `tests/wrapped_command_parse_tests.rs`; these are the checks that need
//! crate-private reach.

use std::path::{Path, PathBuf};

use rstest::rstest;

use kaish_types::{ToolArgs, Value};

use super::constraint::{path_is_under, resolve_under};
use super::declaration::{Flag, Positional, Stdin, Style, Tail, Verb, WrappedCommand};
use super::{WrappedError, WrappedTool};

/// Build a tool without touching the filesystem. `build()` verifies the
/// executable; every test here is about the declaration's grammar, not about
/// which binary it pins.
fn tool(declaration: WrappedCommand) -> WrappedTool {
    WrappedTool::from_parts(declaration, PathBuf::from("/usr/bin/probe"))
}

fn args(words: &[&str]) -> ToolArgs {
    let mut args = ToolArgs::new();
    for word in words {
        args.positional.push(Value::String((*word).to_string()));
    }
    args
}

fn git() -> WrappedCommand {
    WrappedCommand::new("git")
        .executable("/usr/bin/git")
        .lead(["--no-pager"])
        .verb(
            Verb::new("log")
                .flag(Flag::value("max-count").alias("-n").int())
                .flag(Flag::switch("oneline"))
                .flag(Flag::value("since"))
                .positional(Positional::many("revision")),
        )
        .verb(
            Verb::new("commit")
                .flag(Flag::value("message").alias("-m").repeatable().required())
                .positional(Positional::many("pathspec")),
        )
}

/// Create a real executable file so `build()`'s verification has something to
/// accept.
fn executable_at(dir: &Path, name: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, "#!/bin/sh\nexit 0\n").unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    }
    path
}

// ── build(): the executable ────────────────────────────────────────────

#[test]
fn build_accepts_a_verified_executable() {
    let dir = tempfile::tempdir().unwrap();
    let path = executable_at(dir.path(), "probe");
    let built = WrappedCommand::new("probe")
        .executable(&path)
        .root(Verb::root())
        .build()
        .expect("a real executable should build");
    assert_eq!(built.executable(), path.as_path());
}

#[test]
fn build_rejects_a_relative_executable() {
    let error = WrappedCommand::new("probe")
        .executable("probe")
        .root(Verb::root())
        .build()
        .expect_err("a relative path names no pinned install");
    let message = error.to_string();
    assert!(message.contains("probe"), "{message}");
    assert!(message.contains("absolute"), "{message}");
}

#[test]
fn build_rejects_a_missing_executable() {
    let dir = tempfile::tempdir().unwrap();
    let missing = dir.path().join("absent");
    let error = WrappedCommand::new("probe")
        .executable(&missing)
        .root(Verb::root())
        .build()
        .expect_err("a missing path cannot be pinned");
    let message = error.to_string();
    assert!(message.contains(&missing.display().to_string()), "{message}");
}

#[test]
fn build_rejects_a_directory() {
    let dir = tempfile::tempdir().unwrap();
    let error = WrappedCommand::new("probe")
        .executable(dir.path())
        .root(Verb::root())
        .build()
        .expect_err("a directory is not an executable");
    assert!(error.to_string().contains("is not a file"), "{error}");
}

#[cfg(unix)]
#[test]
fn build_rejects_a_file_without_an_execute_bit() {
    use std::os::unix::fs::PermissionsExt;
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("inert");
    std::fs::write(&path, "not executable").unwrap();
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o644)).unwrap();

    let error = WrappedCommand::new("probe")
        .executable(&path)
        .root(Verb::root())
        .build()
        .expect_err("a file with no execute bit cannot run");
    assert!(error.to_string().contains("execute bit"), "{error}");
}

// ── build(): the declaration's grammar ─────────────────────────────────

#[test]
fn build_rejects_a_duplicate_verb() {
    let dir = tempfile::tempdir().unwrap();
    let path = executable_at(dir.path(), "probe");
    let error = WrappedCommand::new("probe")
        .executable(&path)
        .verb(Verb::new("run"))
        .verb(Verb::new("run"))
        .build()
        .expect_err("two verbs cannot share a name");
    assert!(error.to_string().contains("'run' twice"), "{error}");
}

#[rstest]
#[case::same_name(Flag::switch("force"), Flag::switch("force"), "--force")]
#[case::alias_shadows_name(Flag::switch("s"), Flag::switch("short").alias("-s"), "-s")]
#[case::two_aliases(
    Flag::value("a").alias("-x"),
    Flag::value("b").alias("-x"),
    "-x"
)]
fn build_rejects_a_duplicate_flag_spelling(
    #[case] first: Flag,
    #[case] second: Flag,
    #[case] spelling: &str,
) {
    let dir = tempfile::tempdir().unwrap();
    let path = executable_at(dir.path(), "probe");
    let error = WrappedCommand::new("probe")
        .executable(&path)
        .verb(Verb::new("run").flag(first).flag(second))
        .build()
        .expect_err("two flags cannot share a spelling");
    let message = error.to_string();
    assert!(message.contains(spelling), "{message}");
    assert!(message.contains("twice"), "{message}");
}

#[test]
fn build_rejects_a_many_positional_that_is_not_last() {
    let dir = tempfile::tempdir().unwrap();
    let path = executable_at(dir.path(), "probe");
    let error = WrappedCommand::new("probe")
        .executable(&path)
        .verb(
            Verb::new("run")
                .positional(Positional::many("files"))
                .positional(Positional::one("target")),
        )
        .build()
        .expect_err("a many positional absorbs the rest, so nothing can follow it");
    let message = error.to_string();
    assert!(message.contains("files"), "{message}");
    assert!(message.contains("target"), "{message}");
}

#[test]
fn build_rejects_a_required_positional_after_an_optional_one() {
    let dir = tempfile::tempdir().unwrap();
    let path = executable_at(dir.path(), "probe");
    let error = WrappedCommand::new("probe")
        .executable(&path)
        .verb(
            Verb::new("run")
                .positional(Positional::one("maybe"))
                .positional(Positional::one("must").required()),
        )
        .build()
        .expect_err("no call could fill the optional slot without the required one");
    let message = error.to_string();
    assert!(message.contains("must"), "{message}");
    assert!(message.contains("maybe"), "{message}");
}

#[test]
fn build_accepts_a_root_verb_beside_named_verbs() {
    let dir = tempfile::tempdir().unwrap();
    let path = executable_at(dir.path(), "python3");
    WrappedCommand::new("python")
        .executable(&path)
        .root(Verb::root().positional(Positional::one("script")))
        .verb(Verb::new("json-tool").positional(Positional::one("file")))
        .build()
        .expect("the python interpreter posture declares both");
}

#[test]
fn build_rejects_a_declaration_with_no_verb_at_all() {
    let dir = tempfile::tempdir().unwrap();
    let path = executable_at(dir.path(), "probe");
    let error = WrappedCommand::new("probe")
        .executable(&path)
        .build()
        .expect_err("a declaration that names no verb can accept no call");
    assert!(error.to_string().contains("no verbs and no root"), "{error}");
}

#[test]
fn build_rejects_choices_on_a_switch() {
    let dir = tempfile::tempdir().unwrap();
    let path = executable_at(dir.path(), "probe");
    let error = WrappedCommand::new("probe")
        .executable(&path)
        .verb(Verb::new("run").flag(Flag::switch("mode").choices(["a", "b"])))
        .build()
        .expect_err("a switch binds no value, so a set of values means nothing");
    assert!(error.to_string().contains("binds no value"), "{error}");
}

#[test]
fn build_rejects_a_relative_path_under_root() {
    let dir = tempfile::tempdir().unwrap();
    let path = executable_at(dir.path(), "probe");
    let error = WrappedCommand::new("probe")
        .executable(&path)
        .root(Verb::root().positional(Positional::one("script").path_under("scripts")))
        .build()
        .expect_err("a relative root cannot be resolved against anything stable");
    assert!(error.to_string().contains("absolute path"), "{error}");
}

// ── The declaration is data ────────────────────────────────────────────

#[test]
fn a_declaration_round_trips_through_json() {
    let declaration = git();
    let wire = serde_json::to_string(&declaration).expect("serialize");
    let back: WrappedCommand = serde_json::from_str(&wire).expect("deserialize");
    assert_eq!(back, declaration);
}

#[test]
fn a_declaration_deserializes_from_the_documented_field_names() {
    // One spelling: the wire names are the Rust field names, and
    // `docs/wrapped_command.md` writes the same plural in its TOML sample.
    let wire = r#"{
        "name": "python",
        "executable": "/usr/bin/python3",
        "lead": ["-I"],
        "root": {
            "tail": "after-dash-dash",
            "stdin": "pipe",
            "positionals": [
                { "name": "script", "required": true, "path_under": "/opt/app/scripts" },
                { "name": "args", "many": true }
            ]
        }
    }"#;
    let declaration: WrappedCommand = serde_json::from_str(wire).expect("deserialize");
    let root = declaration.root.as_ref().expect("root verb");
    assert_eq!(root.tail, Tail::AfterDashDash);
    assert_eq!(root.stdin, Stdin::Pipe);
    assert_eq!(root.positionals.len(), 2);
    assert_eq!(
        root.positionals[0].path_under.as_deref(),
        Some(Path::new("/opt/app/scripts"))
    );
    assert!(root.positionals[1].many);
}

// ── The schema ─────────────────────────────────────────────────────────

#[test]
fn the_schema_binds_raw_argv_and_publishes_verbs_as_subcommands() {
    let schema = tool(git()).schema();
    assert!(schema.raw_argv, "the wrapper owns its own grammar");
    let names: Vec<&str> = schema.subcommands.iter().map(|s| s.name.as_str()).collect();
    assert_eq!(names, vec!["log", "commit"]);
}

#[test]
fn a_flag_publishes_its_name_aliases_and_constraints() {
    let schema = tool(
        WrappedCommand::new("git").verb(
            Verb::new("status")
                .flag(Flag::switch("short").alias("-s"))
                .flag(
                    Flag::value("untracked-files")
                        .about("Which untracked files to report")
                        .choices(["no", "normal", "all"]),
                ),
        ),
    )
    .schema();
    let status = &schema.subcommands[0];

    let short = &status.params[0];
    assert_eq!(short.name, "short");
    assert_eq!(short.param_type, "bool");
    assert_eq!(short.aliases, vec!["-s".to_string()]);

    let untracked = &status.params[1];
    assert_eq!(untracked.param_type, "string");
    assert_eq!(
        untracked.description,
        "Which untracked files to report; one of: no, normal, all"
    );
}

#[test]
fn an_int_flag_publishes_as_int_and_a_repeatable_one_as_repeatable() {
    let schema = tool(git()).schema();
    let log = &schema.subcommands[0];
    let max_count = log.params.iter().find(|p| p.name == "max-count").unwrap();
    assert_eq!(max_count.param_type, "int");
    assert_eq!(max_count.aliases, vec!["-n".to_string()]);

    let commit = &schema.subcommands[1];
    let message = commit.params.iter().find(|p| p.name == "message").unwrap();
    assert!(message.repeatable);
    assert!(message.required);
}

#[test]
fn a_path_constrained_positional_publishes_its_root() {
    let schema = tool(
        WrappedCommand::new("python").root(
            Verb::root()
                .positional(Positional::one("script").required().path_under("/opt/app/scripts")),
        ),
    )
    .schema();
    let script = &schema.params[0];
    assert!(script.positional);
    assert!(script.required);
    assert_eq!(script.description, "must be under /opt/app/scripts");
}

#[test]
fn a_forwarding_verb_marks_itself_in_its_description() {
    let schema = tool(
        WrappedCommand::new("cargo").verb(Verb::new("clippy").tail(Tail::Forward)),
    )
    .schema();
    assert!(
        schema.subcommands[0]
            .description
            .ends_with("forwards undeclared flags"),
        "{}",
        schema.subcommands[0].description
    );
}

#[test]
fn examples_from_the_declaration_reach_the_schema() {
    let schema = tool(
        WrappedCommand::new("git")
            .example("Recent history", "git log -n 5 --oneline")
            .verb(Verb::new("log").example("One line each", "git log --oneline")),
    )
    .schema();
    assert_eq!(schema.examples[0].code, "git log -n 5 --oneline");
    assert_eq!(schema.subcommands[0].examples[0].code, "git log --oneline");
}

#[test]
fn typed_substitution_needs_every_verb_to_declare_json() {
    // The kernel reads `typed_substitution` off the ROOT schema, so a tool
    // with one JSON verb among text ones must not claim it — `$(cargo build)`
    // would bind a value that does not exist. The verb keeps the flag on its
    // own schema so `help` still reports it.
    let mixed = tool(
        WrappedCommand::new("cargo")
            .verb(Verb::new("build"))
            .verb(Verb::new("metadata").json_output()),
    )
    .schema();
    assert!(!mixed.typed_substitution, "a text verb keeps the root off");
    assert!(mixed.subcommands[1].typed_substitution, "the verb still says so");

    let all_json = tool(
        WrappedCommand::new("meta").verb(Verb::new("read").json_output()),
    )
    .schema();
    assert!(all_json.typed_substitution);
}

// ── plan_call ──────────────────────────────────────────────────────────

#[test]
fn plan_call_reports_the_verb_stdin_and_json_output() {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("f.json");
    std::fs::write(&file, "{}").unwrap();

    let declaration = WrappedCommand::new("python").verb(
        Verb::new("json-tool")
            .lead(["-m", "json.tool"])
            .omit_name()
            .positional(Positional::one("file"))
            .stdin(Stdin::Pipe)
            .json_output(),
    );
    let planned = tool(declaration)
        .plan_call(&args(&["json-tool", &file.display().to_string()]))
        .expect("a declared verb should plan");

    assert_eq!(planned.verb.as_deref(), Some("json-tool"));
    assert_eq!(planned.stdin, Stdin::Pipe);
    assert!(planned.json_output);
    assert_eq!(
        planned.argv,
        vec!["-m", "json.tool", &file.display().to_string()]
    );
}

#[test]
fn a_binary_word_is_refused_by_name_not_stringified() {
    let mut call = args(&["log"]);
    call.positional.push(Value::Bytes(vec![0xff, 0x00, 0xfe]));
    let error = tool(git())
        .plan_call(&call)
        .expect_err("binary does not cross the argv text boundary");
    assert_eq!(
        error,
        WrappedError::BinaryArgument {
            command: "git".into(),
            position: 2,
            byte_len: 3,
        }
    );
    assert_eq!(error.exit_code(), 2);
}

#[test]
fn a_nul_byte_is_refused_at_the_parse_naming_the_argument() {
    let error = tool(git())
        .plan_call(&args(&["log", "a\0b"]))
        .expect_err("argv cannot carry a NUL byte");
    assert_eq!(
        error.to_string(),
        "git: argument 2 contains a NUL byte. argv cannot carry NUL; remove it."
    );
    assert_eq!(error.exit_code(), 2);
}

// ── validate: what an opaque word does and does not license ────────────

/// The placeholder `build_tool_args_for_validation` writes for a word it
/// could not evaluate.
const DYNAMIC: &str = "<dynamic>";

#[test]
fn a_literal_unknown_flag_is_an_error_before_anything_runs() {
    let issues = tool(git()).validate(&args(&["log", "--output=/tmp/x"]));
    assert_eq!(issues.len(), 1, "{issues:?}");
    assert_eq!(issues[0].severity, kaish_tool_api::Severity::Error);
    assert_eq!(
        issues[0].message,
        "git: unknown flag '--output' for 'git log'. Allowed: -n/--max-count, --oneline, --since"
    );
}

#[test]
fn an_opaque_word_cannot_be_an_unknown_flag() {
    // `git log "$branch"` — the validator has no value, so it has no verdict.
    // Execution parses the expanded text like a literal and refuses it there.
    let issues = tool(git()).validate(&args(&["log", DYNAMIC]));
    assert!(issues.is_empty(), "{issues:?}");
}

#[test]
fn an_opaque_flag_value_still_lets_the_flag_name_be_judged() {
    // `git log --output=$x`: the name came from the source even though the
    // value did not.
    let issues = tool(git()).validate(&args(&["log", "--output=<dynamic>"]));
    assert_eq!(issues.len(), 1, "{issues:?}");
    assert_eq!(issues[0].severity, kaish_tool_api::Severity::Error);
}

#[test]
fn an_opaque_value_cannot_fail_a_choices_set() {
    let declaration = WrappedCommand::new("git").verb(
        Verb::new("status").flag(Flag::value("untracked-files").choices(["no", "normal", "all"])),
    );
    let checked = tool(declaration);
    assert!(checked
        .validate(&args(&["status", "--untracked-files=<dynamic>"]))
        .is_empty());
    let refused = checked.validate(&args(&["status", "--untracked-files=maybe"]));
    assert_eq!(refused.len(), 1, "{refused:?}");
    assert_eq!(refused[0].code, kaish_tool_api::IssueCode::InvalidArgType);
}

#[test]
fn an_opaque_word_in_positional_position_softens_what_follows() {
    // `$x` could have expanded to `--`, which would make `--output=x` a
    // path. The reading that refuses it is still reported, as a warning.
    let issues = tool(git()).validate(&args(&["log", DYNAMIC, "--output=x"]));
    assert_eq!(issues.len(), 1, "{issues:?}");
    assert_eq!(issues[0].severity, kaish_tool_api::Severity::Warning);
}

#[test]
fn an_opaque_verb_yields_no_verdict_at_all() {
    let issues = tool(git()).validate(&args(&[DYNAMIC, "--output=x"]));
    assert!(issues.is_empty(), "{issues:?}");
}

#[test]
fn a_missing_required_flag_is_reported_at_validation() {
    let issues = tool(git()).validate(&args(&["commit"]));
    assert_eq!(issues.len(), 1, "{issues:?}");
    assert_eq!(issues[0].code, kaish_tool_api::IssueCode::MissingRequiredArg);
    assert_eq!(
        issues[0].message,
        "git: required flag '--message' not given for 'git commit'."
    );
}

#[test]
fn an_opaque_word_suppresses_the_required_check() {
    // The opaque word may have been `-m`, which would carry the message.
    let issues = tool(git()).validate(&args(&["commit", DYNAMIC]));
    assert!(issues.is_empty(), "{issues:?}");
}

#[test]
fn an_unknown_verb_carries_the_new_issue_code() {
    let issues = tool(git()).validate(&args(&["comit"]));
    assert_eq!(issues.len(), 1, "{issues:?}");
    assert_eq!(
        issues[0].code,
        kaish_tool_api::IssueCode::WrappedCallRejected
    );
    assert_eq!(issues[0].code.code(), "E021");
}

// ── path_is_under and resolve_under ────────────────────────────────────

#[test]
fn path_is_under_compares_components_not_string_prefixes() {
    assert!(path_is_under(
        Path::new("/opt/app/scripts/etl.py"),
        Path::new("/opt/app/scripts")
    ));
    assert!(
        path_is_under(Path::new("/opt/app/scripts"), Path::new("/opt/app/scripts")),
        "the root itself is inside the root"
    );
    assert!(
        !path_is_under(
            Path::new("/opt/app/scripts-evil/x"),
            Path::new("/opt/app/scripts")
        ),
        "a sibling that shares a string prefix is not inside"
    );
    assert!(!path_is_under(Path::new("/opt/app"), Path::new("/opt/app/scripts")));
}

/// A root directory with a script in it, plus the cwd every relative value
/// resolves against.
struct Sandbox {
    _dir: tempfile::TempDir,
    root: PathBuf,
    outside: PathBuf,
}

fn sandbox() -> Sandbox {
    let dir = tempfile::tempdir().unwrap();
    let base = std::fs::canonicalize(dir.path()).unwrap();
    let root = base.join("scripts");
    std::fs::create_dir(&root).unwrap();
    std::fs::write(root.join("etl.py"), "").unwrap();
    let outside = base.join("elsewhere");
    std::fs::create_dir(&outside).unwrap();
    std::fs::write(outside.join("evil.py"), "").unwrap();
    // The sibling-prefix hazard: `scripts-evil` shares a string prefix with
    // `scripts` and must not be inside it.
    let sibling = base.join("scripts-evil");
    std::fs::create_dir(&sibling).unwrap();
    std::fs::write(sibling.join("x.py"), "").unwrap();
    Sandbox {
        _dir: dir,
        root,
        outside,
    }
}

#[test]
fn resolve_under_accepts_a_relative_value_under_the_cwd() {
    let sandbox = sandbox();
    let resolved = resolve_under("etl.py", &sandbox.root, &sandbox.root)
        .expect("a file in the root resolves");
    assert_eq!(resolved, sandbox.root.join("etl.py"));
}

#[test]
fn resolve_under_rejects_a_dot_dot_traversal() {
    let sandbox = sandbox();
    let error = resolve_under("../elsewhere/evil.py", &sandbox.root, &sandbox.root)
        .expect_err("`..` walks out of the root");
    assert!(matches!(error, WrappedError::PathOutsideRoot { .. }), "{error:?}");
}

#[test]
fn resolve_under_rejects_an_absolute_value_outside_the_root() {
    let sandbox = sandbox();
    let outside = sandbox.outside.join("evil.py");
    let error = resolve_under(&outside.display().to_string(), &sandbox.root, &sandbox.root)
        .expect_err("an absolute path outside the root is outside it");
    assert!(matches!(error, WrappedError::PathOutsideRoot { .. }), "{error:?}");
}

#[test]
fn resolve_under_rejects_a_sibling_that_shares_a_string_prefix() {
    let sandbox = sandbox();
    let sibling = sandbox
        .root
        .parent()
        .unwrap()
        .join("scripts-evil")
        .join("x.py");
    let error = resolve_under(&sibling.display().to_string(), &sandbox.root, &sandbox.root)
        .expect_err("scripts-evil is not under scripts");
    assert!(matches!(error, WrappedError::PathOutsideRoot { .. }), "{error:?}");
}

#[cfg(unix)]
#[test]
fn resolve_under_follows_a_symlink_out_of_the_root() {
    let sandbox = sandbox();
    std::os::unix::fs::symlink(sandbox.outside.join("evil.py"), sandbox.root.join("link.py"))
        .unwrap();
    let error = resolve_under("link.py", &sandbox.root, &sandbox.root)
        .expect_err("a symlink whose target leaves the root fails");
    assert!(matches!(error, WrappedError::PathOutsideRoot { .. }), "{error:?}");
}

#[cfg(unix)]
#[test]
fn resolve_under_accepts_a_root_that_is_itself_a_symlink() {
    let sandbox = sandbox();
    let alias = sandbox.root.parent().unwrap().join("alias");
    std::os::unix::fs::symlink(&sandbox.root, &alias).unwrap();
    let resolved = resolve_under("etl.py", &sandbox.root, &alias)
        .expect("the root canonicalizes to the same directory");
    assert_eq!(resolved, sandbox.root.join("etl.py"));
}

#[test]
fn resolve_under_rejects_a_relative_value_when_the_cwd_is_outside_the_root() {
    let sandbox = sandbox();
    let error = resolve_under("evil.py", &sandbox.outside, &sandbox.root)
        .expect_err("a cwd outside the root cannot produce a path inside it");
    assert!(matches!(error, WrappedError::PathOutsideRoot { .. }), "{error:?}");
}

#[test]
fn resolve_under_accepts_a_file_that_does_not_exist_yet_inside_the_root() {
    let sandbox = sandbox();
    let resolved = resolve_under("new.py", &sandbox.root, &sandbox.root)
        .expect("a path that does not exist yet still resolves lexically");
    assert_eq!(resolved, sandbox.root.join("new.py"));
}

#[test]
fn resolve_under_rejects_a_root_that_does_not_resolve() {
    let sandbox = sandbox();
    let missing = sandbox.root.join("absent");
    let error = resolve_under("x.py", &sandbox.root, &missing)
        .expect_err("containment cannot be proven against a root that is not there");
    assert!(
        matches!(error, WrappedError::PathRootUnresolvable { .. }),
        "{error:?}"
    );
}

#[test]
fn plan_call_rejects_an_absolute_literal_outside_the_root_before_anything_spawns() {
    let sandbox = sandbox();
    let declaration = WrappedCommand::new("python").root(
        Verb::root()
            .positional(Positional::one("script").required().path_under(&sandbox.root))
            .positional(Positional::many("args")),
    );
    let outside = sandbox.outside.join("evil.py");
    let error = tool(declaration)
        .plan_call(&args(&[&outside.display().to_string()]))
        .expect_err("an absolute path outside the root is decided at plan time");
    assert!(
        error
            .to_string()
            .starts_with(&format!("python: 'script' must be under {}", sandbox.root.display())),
        "{error}"
    );
}

#[test]
fn plan_call_rewrites_an_absolute_path_to_its_resolved_form() {
    let sandbox = sandbox();
    let declaration = WrappedCommand::new("python")
        .lead(["-I"])
        .root(Verb::root().positional(Positional::one("script").path_under(&sandbox.root)));
    let written = sandbox.root.join("etl.py");
    let planned = tool(declaration)
        .plan_call(&args(&[&written.display().to_string()]))
        .expect("a path inside the root plans");
    assert_eq!(planned.argv, vec!["-I", &written.display().to_string()]);
    assert_eq!(planned.path_checks.len(), 1);
    assert!(planned.path_checks[0].resolved);
    assert_eq!(planned.path_checks[0].argv_index, 1);
}

#[test]
fn plan_call_defers_a_relative_path_to_the_execute_lane() {
    let sandbox = sandbox();
    let declaration = WrappedCommand::new("python")
        .root(Verb::root().positional(Positional::one("script").path_under(&sandbox.root)));
    let checked = tool(declaration);
    let planned = checked
        .plan_call(&args(&["etl.py"]))
        .expect("a relative path is not decided without the real cwd");
    assert_eq!(planned.argv, vec!["etl.py"]);
    assert_eq!(planned.path_checks.len(), 1);
    assert!(!planned.path_checks[0].resolved);

    let resolved = checked
        .resolve_path_check(&planned.path_checks[0], &sandbox.root)
        .expect("the execute lane finishes the check");
    assert_eq!(resolved, sandbox.root.join("etl.py"));
}

// ── Style overrides ────────────────────────────────────────────────────

#[rstest]
#[case::long_default(Flag::value("since"), vec!["--since=2d"])]
#[case::short_default(Flag::value("m"), vec!["-m", "2d"])]
#[case::long_forced_separate(
    Flag::value("since").style(Style::Separate),
    vec!["--since", "2d"]
)]
#[case::short_forced_equals(Flag::value("m").style(Style::Equals), vec!["-m=2d"])]
fn a_style_override_replaces_the_default_for_the_name_length(
    #[case] flag: Flag,
    #[case] expected: Vec<&str>,
) {
    let written = flag.written_name();
    let declaration = WrappedCommand::new("probe").root(Verb::root().flag(flag));
    let planned = tool(declaration)
        .plan_call(&args(&[&written, "2d"]))
        .expect("a declared flag plans");
    assert_eq!(planned.argv, expected);
}

// ── The two binders agree ──────────────────────────────────────────────

/// The reduced evaluator the agreement test drives both binders with:
/// literals evaluate to themselves, and `$branch` evaluates to the text a
/// shell would expand it to.
struct FixedSource;

#[async_trait::async_trait]
impl crate::kernel::ArgValueSource for FixedSource {
    async fn eval(&self, expr: &crate::ast::Expr) -> anyhow::Result<Option<Value>> {
        Ok(Some(match expr {
            crate::ast::Expr::Literal(value) => value.clone(),
            crate::ast::Expr::VarRef(_) => Value::String("--output=/etc/cron.d/x".to_string()),
            other => anyhow::bail!("the agreement fixture does not evaluate {other:?}"),
        }))
    }

    async fn expand_glob(&self, _pattern: &str) -> anyhow::Result<Option<Vec<String>>> {
        Ok(None)
    }

    async fn home(&self) -> Option<String> {
        None
    }
}

fn literal(word: &str) -> crate::ast::Arg {
    crate::ast::Arg::Positional(crate::ast::Expr::Literal(Value::String(word.to_string())))
}

fn variable(name: &str) -> crate::ast::Arg {
    crate::ast::Arg::Positional(crate::ast::Expr::VarRef(crate::ast::VarPath::simple(name)))
}

/// The two binders must hand the wrapper the same shape for the same source.
///
/// `Tool::validate` reads what the validator's binder produced and
/// `Tool::execute` reads what the kernel's binder produced. When they
/// disagree, `validate` judges a call that will never run — the two-binders
/// hazard. Under `raw_argv` both must keep every word, in source order, in
/// `positional`.
#[rstest]
#[case::plain(vec![literal("log"), literal("--oneline")])]
#[case::value_flag(vec![literal("log"), literal("-n"), literal("5")])]
#[case::dash_dash(vec![literal("log"), crate::ast::Arg::DoubleDash, literal("main")])]
#[case::short_flag(vec![literal("log"), crate::ast::Arg::ShortFlag("n".into()), literal("5")])]
#[case::long_flag(vec![literal("log"), crate::ast::Arg::LongFlag("oneline".into())])]
#[case::named(vec![
    literal("log"),
    crate::ast::Arg::Named { key: "since".into(), value: crate::ast::Expr::Literal(Value::String("2d".into())) },
])]
#[tokio::test]
async fn the_two_binders_agree_on_every_literal_shape(#[case] argv: Vec<crate::ast::Arg>) {
    let checked = tool(git());
    let schema = checked.schema();

    let executed = crate::kernel::bind_tool_args(&argv, Some(&schema), &FixedSource)
        .await
        .expect("the execution binder should bind");
    let validated = crate::validator::build_tool_args_for_validation(&argv, Some(&schema));

    assert_eq!(
        executed.positional, validated.positional,
        "the two binders must hand the wrapper the same words"
    );
    assert_eq!(
        checked.validate(&validated).is_empty(),
        checked.plan_call(&executed).is_ok(),
        "the two binders must reach the same verdict"
    );
}

/// The one shape where the binders genuinely differ: a variable's value is
/// known to execution and not to validation. Validation must stay quiet, and
/// execution must refuse — the same call, judged when its value exists.
#[tokio::test]
async fn a_variable_is_opaque_to_validation_and_refused_at_execution() {
    let argv = vec![literal("log"), variable("branch")];
    let checked = tool(git());
    let schema = checked.schema();

    let validated = crate::validator::build_tool_args_for_validation(&argv, Some(&schema));
    assert!(
        checked.validate(&validated).is_empty(),
        "validation has no value to judge"
    );

    let executed = crate::kernel::bind_tool_args(&argv, Some(&schema), &FixedSource)
        .await
        .expect("the execution binder should bind");
    let error = checked
        .plan_call(&executed)
        .expect_err("the expanded value is parsed like a literal");
    assert_eq!(
        error.to_string(),
        "git: unknown flag '--output' for 'git log'. Allowed: -n/--max-count, --oneline, --since"
    );
}
