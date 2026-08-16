//! Heredoc fragments: what a command is fed on stdin, published as data, and
//! expanded against a scope the caller supplies.
//!
//! Agents hand whole programs to interpreters through heredocs — `python3
//! <<'PY'`, `sqlite3 <<SQL`. A plan already names the command; these tests
//! pin the body it reads, the delimiter word that hints at its language, and
//! whether the body is literal. `expand_fragment` closes the loop: it
//! resolves the pure expansions against caller-supplied values and refuses,
//! loudly, when a `$(...)` means the text it could produce would not be the
//! text that runs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{expand_fragment, plan_program, Expansion, FragmentAddr, FragmentError};
use kaish_types::plan::{Plan, PlannedHeredoc};
use kaish_types::Value;

/// Every heredoc the first statement carries, in source order.
fn heredocs(source: &str) -> Vec<PlannedHeredoc> {
    plan_program(source).expect("parses")[0]
        .plan
        .commands
        .iter()
        .flat_map(|c| c.heredocs.clone())
        .collect()
}

fn vars(pairs: &[(&str, &str)]) -> Vec<(String, Value)> {
    pairs
        .iter()
        .map(|(k, v)| ((*k).to_string(), Value::String((*v).to_string())))
        .collect()
}

fn expand(source: &str, heredoc: usize, scope: &[(String, Value)]) -> Expansion {
    expand_fragment(source, FragmentAddr::new(0, heredoc), scope).expect("expands")
}

// ───────────────────────── What the plan publishes ─────────────────────────

/// The shape this feature exists for: the interpreter, the language hint, and
/// the program, all as data — no shell framing left to strip.
#[test]
fn a_literal_heredoc_publishes_its_command_delimiter_and_body() {
    let plans = plan_program("python3 <<'PY'\nimport os\nprint(os.getcwd())\nPY").expect("parses");
    let command = &plans[0].plan.commands[0];
    assert_eq!(command.name, "python3");

    let heredoc = &command.heredocs[0];
    assert_eq!(heredoc.delimiter, "PY");
    assert!(heredoc.literal, "a quoted delimiter is a literal body");
    assert!(!heredoc.strip_tabs);
    assert_eq!(heredoc.body.display(), "import os\nprint(os.getcwd())\n");
}

/// The delimiter word is the language hint agents actually write, so it must
/// survive planning. It used to be rewritten to `EOF` on the way out.
#[test]
fn the_delimiter_word_survives_planning() {
    for (source, expected) in [
        ("python3 <<'PY'\nx\nPY", "PY"),
        ("sqlite3 db <<SQL\nselect 1;\nSQL", "SQL"),
        ("node <<'JS'\nlet x\nJS", "JS"),
    ] {
        assert_eq!(heredocs(source)[0].delimiter, expected, "source: {source}");
    }
}

/// An unquoted delimiter means the shell expands the body before the command
/// sees it — the text a reader analyzes is not the text that runs, and the
/// flag saying so is the whole safety distinction.
#[test]
fn an_unquoted_delimiter_reports_a_non_literal_body_and_names_its_variables() {
    let heredoc = &heredocs("python3 <<PY\nname = \"${NAME}\"\nPY")[0];
    assert!(!heredoc.literal, "an unquoted delimiter interpolates");
    assert_eq!(heredoc.free_variables, vec!["NAME".to_string()]);
    // Unexpanded, like every other plan surface: `${NAME}` reads as written.
    assert!(heredoc.body.display().contains("${NAME}"));
}

/// A literal body reads no variables however many `$` it contains — the
/// quoted delimiter is what decides, not the text.
#[test]
fn a_literal_body_reads_no_variables() {
    let heredoc = &heredocs("python3 <<'PY'\nname = \"${NAME}\"\nPY")[0];
    assert!(heredoc.free_variables.is_empty());
}

/// `<<-` strips leading tabs at execution. The published body stays verbatim
/// and the flag names the transform, so a reader is never guessing which one
/// it has.
#[test]
fn the_dash_form_reports_tab_stripping_and_keeps_the_body_verbatim() {
    let heredoc = &heredocs("python3 <<-'PY'\n\timport os\nPY")[0];
    assert!(heredoc.strip_tabs);
    assert_eq!(heredoc.body.display(), "\timport os\n");
}

/// The body is the source spelling, never a kernel-internal rewrite. The
/// lexer rewrites `$((…))` inside an interpolated body to a private
/// `${__ARITH:…}` form; that spelling must not reach an embedder.
#[test]
fn an_arithmetic_body_publishes_the_source_spelling() {
    let body = heredocs("python3 <<PY\nn = $((1 + 2))\nPY")[0].body.display();
    assert!(body.contains("$((1 + 2))"), "body was: {body}");
    assert!(!body.contains("__ARITH"), "kernel internal leaked: {body}");
}

/// Heredocs are addressed by a flat per-statement index, so one inside a loop
/// body is reachable — the body is exactly where a generated program lands.
#[test]
fn a_heredoc_inside_a_loop_body_is_published_and_addressable() {
    let source = "for f in a b; do python3 <<'PY'\nimport os\nPY\ndone";
    let found = heredocs(source);
    assert_eq!(found.len(), 1);
    assert_eq!(found[0].index, 0);
    assert_eq!(
        expand(source, 0, &[]),
        Expansion::Complete("import os\n".to_string())
    );
}

/// One statement, several commands, one flat numbering: the index is what
/// makes a heredoc addressable without the caller walking the structure that
/// contains it.
#[test]
fn heredocs_are_numbered_flat_across_a_statement() {
    let source = concat!(
        "if [[ -f a ]]; then\n",
        "python3 <<'FIRST'\nleft\nFIRST\n",
        "else\n",
        "python3 <<'SECOND'\nright\nSECOND\n",
        "fi"
    );
    let found = heredocs(source);
    assert_eq!(
        found.iter().map(|h| h.delimiter.as_str()).collect::<Vec<_>>(),
        vec!["FIRST", "SECOND"]
    );
    assert_eq!(found.iter().map(|h| h.index).collect::<Vec<_>>(), vec![0, 1]);
    // Both walks agree on the numbering, so the plan's index is the address.
    assert_eq!(
        expand(source, 1, &[]),
        Expansion::Complete("right\n".to_string())
    );
}

/// Two heredocs on one command never reach a plan: kaish rejects several
/// stdin sources on one command rather than picking one. Pinned here because
/// the flat numbering above would otherwise look like it had to handle it.
#[test]
fn two_heredocs_on_one_command_are_a_parse_error() {
    let errors = plan_program("diff <<'A' <<'B'\nleft\nA\nright\nB").expect_err("ambiguous stdin");
    assert!(
        errors[0].message.contains("multiple stdin redirects"),
        "got: {}",
        errors[0].message
    );
}

/// A command with no heredoc publishes none — the field is not a place where
/// other redirects turn up.
#[test]
fn other_redirects_are_not_heredocs() {
    assert!(heredocs("python3 < script.py > out.txt").is_empty());
    assert!(heredocs("python3 <<< 'print(1)'").is_empty());
}

// ───────────────────────── Expanding a fragment ─────────────────────────

/// A literal body needs no scope at all: what is published is what runs.
#[test]
fn expanding_a_literal_body_returns_it_verbatim() {
    assert_eq!(
        expand("python3 <<'PY'\nimport os\nPY", 0, &[]),
        Expansion::Complete("import os\n".to_string())
    );
}

/// The point of the whole exercise: the caller supplies the values, and gets
/// back the source the interpreter would actually see.
#[test]
fn expanding_an_interpolated_body_substitutes_supplied_values() {
    assert_eq!(
        expand(
            "python3 <<PY\nname = \"${NAME}\"\nPY",
            0,
            &vars(&[("NAME", "world")])
        ),
        Expansion::Complete("name = \"world\"\n".to_string())
    );
}

/// Expansion runs the interpreter's own evaluator, so `<<-` tab stripping
/// happens the way execution does it — stripped from the literal source, not
/// from tabs that arrived through a variable.
#[test]
fn expanding_the_dash_form_strips_tabs_the_way_execution_does() {
    assert_eq!(
        expand(
            "python3 <<-PY\n\tname = \"${NAME}\"\nPY",
            0,
            &vars(&[("NAME", "\tindented")])
        ),
        Expansion::Complete("name = \"\tindented\"\n".to_string()),
    );
}

/// The caller's scope is the only scope. Nothing is read from session state,
/// so an embedder deciding against a value it holds gets the body that value
/// produces — and never a stale one the kernel peeked.
#[test]
fn expansion_reads_only_the_supplied_scope() {
    let source = "python3 <<PY\nprint(\"${NAME}\")\nPY";
    assert_eq!(
        expand(source, 0, &vars(&[("NAME", "first")])),
        Expansion::Complete("print(\"first\")\n".to_string())
    );
    assert_eq!(
        expand(source, 0, &vars(&[("NAME", "second")])),
        Expansion::Complete("print(\"second\")\n".to_string())
    );
}

/// An unsupplied variable expands to empty, because that is what kaish does
/// when it executes — `cat <<PY` with an unset `${NOPE}` prints an empty
/// slot and exits 0. Expansion follows execution rather than being stricter
/// than it: a rule that disagreed here would hand back a body the command
/// never sees.
///
/// So `Complete` means "this is what runs", never "everything was supplied".
/// The guard for the second question is `free_variables`, which is why the
/// plan publishes it per heredoc.
#[test]
fn an_unsupplied_variable_expands_the_way_execution_does() {
    let source = "python3 <<PY\nprint(\"${NAME}\")\nPY";
    assert_eq!(
        expand(source, 0, &[]),
        Expansion::Complete("print(\"\")\n".to_string())
    );
    // The caller that wants the stricter answer has it before expanding.
    assert_eq!(heredocs(source)[0].free_variables, vec!["NAME".to_string()]);
}

// ───────────────────────── `$(...)` is a hole, not a guess ─────────────────

/// A command substitution blocks expansion and returns the question as data:
/// the plan of what it would run. The kernel does not run it — deciding
/// whether that is safe is the caller's whole reason for asking.
#[test]
fn a_command_substitution_blocks_expansion_and_names_what_it_would_run() {
    let expansion = expand("python3 <<PY\nstamp = \"$(date +%s)\"\nPY", 0, &[]);
    let Expansion::Blocked { holes } = expansion else {
        panic!("a $(...) body must not expand to text: {expansion:?}");
    };
    assert_eq!(holes.len(), 1);
    assert_eq!(holes[0].source, "$(date +%s)");
    let plans: &Vec<Plan> = &holes[0].plans;
    assert_eq!(plans[0].commands[0].name, "date");
}

/// Every hole is reported, not just the first — a caller resolving them one
/// at a time must be able to see how many there are.
#[test]
fn every_command_substitution_is_reported() {
    let expansion = expand("python3 <<PY\na = \"$(date)\"\nb = \"$(hostname)\"\nPY", 0, &[]);
    let Expansion::Blocked { holes } = expansion else {
        panic!("expected holes: {expansion:?}");
    };
    assert_eq!(
        holes
            .iter()
            .map(|h| h.plans[0].commands[0].name.as_str())
            .collect::<Vec<_>>(),
        vec!["date", "hostname"]
    );
}

/// A blocked expansion carries no expanded text at all. Half-expanded source
/// handed to a parser reads as ground truth and is not — the variant makes
/// that unrepresentable.
#[test]
fn a_blocked_expansion_carries_no_text() {
    let expansion = expand(
        "python3 <<PY\nname = \"${NAME}\"\nstamp = \"$(date)\"\nPY",
        0,
        &vars(&[("NAME", "world")]),
    );
    let json = serde_json::to_string(&expansion).expect("serializes");
    assert!(
        !json.contains("world"),
        "a blocked expansion must not carry expanded text: {json}"
    );
}

/// A substitution nested inside a `${VAR:-default}` blocks too. Missing one
/// would expand a body around a hole and call it complete.
#[test]
fn a_nested_command_substitution_still_blocks() {
    let expansion = expand("python3 <<PY\nx = \"${NAME:-$(hostname)}\"\nPY", 0, &[]);
    assert!(
        matches!(expansion, Expansion::Blocked { .. }),
        "expected a hole for the nested substitution: {expansion:?}"
    );
}

// ───────────────────────── Addressing failures are loud ────────────────────

/// An address past the end names what it found instead of expanding nothing.
#[test]
fn an_address_past_the_end_is_a_loud_error() {
    let err = expand_fragment("python3 <<'PY'\nx\nPY", FragmentAddr::new(0, 7), &[])
        .expect_err("out-of-range heredoc index");
    assert!(
        matches!(err, FragmentError::NoSuchHeredoc { .. }),
        "got: {err}"
    );
    let err = expand_fragment("python3 <<'PY'\nx\nPY", FragmentAddr::new(9, 0), &[])
        .expect_err("out-of-range statement index");
    assert!(
        matches!(err, FragmentError::NoSuchStatement { .. }),
        "got: {err}"
    );
}

/// Source that does not parse fails as a parse error, not as an empty
/// expansion.
#[test]
fn unparsable_source_is_a_parse_error() {
    let err = expand_fragment("python3 <<'PY'\nx", FragmentAddr::new(0, 0), &[])
        .expect_err("unterminated heredoc");
    assert!(matches!(err, FragmentError::Parse(_)), "got: {err}");
}

/// A body that depends on session state the caller cannot supply through the
/// scope is loud rather than silently expanded against an empty session.
#[test]
fn a_body_needing_session_state_is_a_loud_error() {
    let err = expand_fragment(
        "python3 <<PY\nlast = $?\nPY",
        FragmentAddr::new(0, 0),
        &[],
    )
    .expect_err("$? cannot come from the supplied scope");
    assert!(
        matches!(err, FragmentError::NeedsSessionState { .. }),
        "got: {err}"
    );
}

// ───────────────── The two walks must agree, or an address lies ────────────

/// The plan numbers heredocs with one walk (`ast::plan`) and `expand_fragment`
/// resolves that number with another (`fragment.rs`). If they ever visit
/// heredocs in a different order, an address resolves to the WRONG body and
/// nothing says so — the worst failure this feature can have.
///
/// Every heredoc below is literal with a body unique across the statement, so
/// expanding an address must return exactly the body the plan published at
/// that index. Any disagreement between the walks shows up as a mismatch.
#[test]
fn every_published_address_resolves_to_the_body_it_published() {
    let shapes = [
        ("python3 <<'A'\none\nA", 1),
        ("python3 <<'A' | grep x\none\nA", 1),
        ("python3 <<'A' | python3 <<'B'\none\nA\ntwo\nB", 2),
        ("if [[ -f x ]]; then python3 <<'A'\none\nA\nelse python3 <<'B'\ntwo\nB\nfi", 2),
        ("for f in a b; do python3 <<'A'\none\nA\ndone", 1),
        ("while [[ -f x ]]; do python3 <<'A'\none\nA\ndone", 1),
        ("case $x in a) python3 <<'A'\none\nA\n;; esac", 1),
        ("python3 <<'A' && python3 <<'B'\none\nA\ntwo\nB", 2),
        ("python3 <<'A' || python3 <<'B'\none\nA\ntwo\nB", 2),
        ("X=1 python3 <<'A'\none\nA", 1),
        ("echo $(python3 <<'A'\none\nA\n)", 1),
        ("out=$(python3 <<'A'\none\nA\n)", 1),
        ("for f in $(python3 <<'A'\none\nA\n); do echo $f; done", 1),
        ("if python3 <<'A'\none\nA\nthen echo y\nfi", 1),
        // Reached only through a REDIRECT TARGET's substitution. The plan
        // walk descends into redirect targets, so a heredoc here is numbered;
        // a resolver that did not descend the same way would hand back the
        // *other* body with nothing saying so.
        ("cmd > $(cat <<'A'\none\nA\n) && cat <<'B'\ntwo\nB", 2),
        // Reached only through a substitution inside a double-quoted string.
        ("echo \"x $(cat <<'A'\none\nA\n) y\" && cat <<'B'\ntwo\nB", 2),
    ];

    for (source, expected) in shapes {
        let Ok(plans) = plan_program(source) else {
            panic!("shape does not parse: {source}");
        };
        // Publishing FEWER heredocs than the shape contains is invisible to
        // the resolve check below — one walk cannot disagree with itself, it
        // can only miss a place. The count is what catches that.
        let published: usize = plans
            .iter()
            .flat_map(|p| p.plan.commands.iter())
            .map(|c| c.heredocs.len())
            .sum();
        assert_eq!(published, expected, "heredocs missed in: {source}");
        for planned in &plans {
            for command in &planned.plan.commands {
                for heredoc in &command.heredocs {
                    assert!(
                        heredoc.literal,
                        "fixture must use quoted delimiters: {source}"
                    );
                    let addr = FragmentAddr::new(planned.index, heredoc.index);
                    let expanded = expand_fragment(source, addr, &[])
                        .unwrap_or_else(|e| panic!("address {addr:?} unresolvable in {source}: {e}"));
                    assert_eq!(
                        expanded,
                        Expansion::Complete(heredoc.body.display()),
                        "walks disagree at index {} of: {source}",
                        heredoc.index,
                    );
                }
            }
        }
    }
}

// ─────────────── Session state cannot hide behind a spelling ──────────────

/// `$((…))` resolves `$?`, `$$` and positionals inside the arithmetic
/// evaluator, so they never become a `StringPart` of their own. Expanding
/// against a fresh scope would read `$?` as 0 and call it the text that runs.
#[test]
fn session_state_inside_arithmetic_is_a_loud_error() {
    for body in [
        "n = $(($? + 1))",
        "n = $(($$))",
        "n = $((${?}))",
        "n = $((${$}))",
        "n = $(($1 + 1))",
    ] {
        let source = format!("python3 <<PY\n{body}\nPY");
        let err = expand_fragment(&source, FragmentAddr::new(0, 0), &[])
            .expect_err("session state must not expand");
        assert!(
            matches!(err, FragmentError::NeedsSessionState { .. }),
            "{body} expanded instead of refusing: {err}"
        );
    }
}

/// An ordinary variable inside arithmetic is not session state — the caller
/// can supply it, so it must not be refused. Without this the check above
/// would be free to block everything and still look correct.
#[test]
fn an_ordinary_variable_inside_arithmetic_still_expands() {
    assert_eq!(
        expand(
            "python3 <<PY\nn = $((COUNT + 1))\nPY",
            0,
            &[("COUNT".to_string(), Value::Int(41))]
        ),
        Expansion::Complete("n = 42\n".to_string())
    );
}

/// The braced `${?}` is a variable path, not the `$?` special form, and the
/// scope resolves its root specially — so the two spellings must refuse
/// alike.
#[test]
fn the_braced_exit_code_is_a_loud_error() {
    let source = "python3 <<PY\nlast = ${?}\nPY";
    let err = expand_fragment(source, FragmentAddr::new(0, 0), &[])
        .expect_err("session state must not expand");
    assert!(
        matches!(err, FragmentError::NeedsSessionState { .. }),
        "got: {err}"
    );
}

/// A heredoc's planned redirect names its delimiter, not a body rendered
/// with a delimiter it never had. The target expression has lost the word by
/// planning time, so rendering from it spelled every heredoc `EOF`.
#[test]
fn a_planned_redirect_names_the_real_delimiter() {
    let plans = plan_program("python3 <<'PY'\nimport os\nPY").expect("parses");
    let redirect = &plans[0].plan.commands[0].redirects[0];
    assert_eq!(redirect.kind, "<<");
    assert_eq!(redirect.target.display(), "'PY'");
}
