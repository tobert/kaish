//! Flag-shaped words that are one argument: `-name=value`, `-Wl,-rpath,/x`,
//! `+x`, and a bare `...`.
//!
//! `--name=value` and `name=value` were already one word. A single-dash
//! `-name=value` split at the `=` and was refused as token pasting, a
//! comma-joined flag list (`-Wl,-rpath,/x`, `sort -k2,2n`) split into
//! several arguments, and `+x` and `...` were parse errors. None of these
//! involve substitution, so none of them is pasting. Real pasting stays
//! refused: `$var` or `$(...)` glued to unquoted text, and a quoted
//! fragment glued to a bare one.

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use kaish_kernel::ast::plan::plan_program;
use rstest::rstest;

/// The argv words of the first command `source` plans: a literal's value, or
/// a non-literal's rendered text.
fn planned_args(source: &str) -> Vec<String> {
    let plans = plan_program(source).unwrap_or_else(|errors| {
        let messages: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
        panic!("{source:?} must parse, got: {}", messages.join("; "))
    });
    let json = serde_json::to_value(&plans).expect("plan serializes");
    let commands = json[0]["plan"]["commands"].as_array().expect("commands");
    // A substitution in an argument plans its own command after this one.
    assert!(!commands.is_empty(), "{source:?} must plan a command: {json}");
    commands[0]["args"]
        .as_array()
        .expect("args")
        .iter()
        .map(|arg| {
            if let Some(value) = arg["literal"]["value"].as_str() {
                value.to_string()
            } else if let Some(text) = arg["plain"].as_str() {
                format!("plain:{text}")
            } else {
                panic!("unexpected argument shape {arg}")
            }
        })
        .collect()
}

fn parse_error(source: &str) -> String {
    let errors = plan_program(source).expect_err("must be a parse error");
    errors.iter().map(|e| e.message.clone()).collect::<Vec<_>>().join("; ")
}

#[rstest]
#[case::latex("pdflatex -interaction=nonstopmode main.tex", &["-interaction=nonstopmode", "main.tex"])]
#[case::latex_double_quoted(r#"pdflatex -interaction="nonstopmode" main.tex"#, &["-interaction=nonstopmode", "main.tex"])]
#[case::latex_single_quoted("pdflatex -interaction='batch mode' main.tex", &["-interaction=batch mode", "main.tex"])]
#[case::ghostscript(
    "gs -sDEVICE=png16m -dFirstPage=1 -sOutputFile=/tmp/x.png in.pdf",
    &["-sDEVICE=png16m", "-dFirstPage=1", "-sOutputFile=/tmp/x.png", "in.pdf"]
)]
#[case::gcc_std("gcc -std=c11 -O2 -fsanitize=address x.c", &["-std=c11", "-O2", "-fsanitize=address", "x.c"])]
#[case::java_define("java -Dkey=value -jar app.jar", &["-Dkey=value", "-jar", "app.jar"])]
#[case::numeral_source_text("x -e=1.50 -n=007", &["-e=1.50", "-n=007"])]
#[case::colon_value("x -a=x:y", &["-a=x:y"])]
#[case::comma_value("x -a=x,y", &["-a=x,y"])]
#[case::empty_quoted_value(r#"x -a="""#, &["-a="])]
#[case::linker_equals("gcc -Wl,-rpath=/x main.o", &["-Wl,-rpath=/x", "main.o"])]
#[case::linker_list("gcc -Wl,-rpath,/opt/lib main.o", &["-Wl,-rpath,/opt/lib", "main.o"])]
#[case::linker_long("gcc -Wl,--as-needed main.o", &["-Wl,--as-needed", "main.o"])]
#[case::linker_non_ascii("gcc -Wl,-rpath,/opt/日本 main.o", &["-Wl,-rpath,/opt/日本", "main.o"])]
#[case::sort_key("sort -k2,2n f", &["-k2,2n", "f"])]
#[case::colon_list("awk -F:a f", &["-F:a", "f"])]
#[case::colon_then_comma("x -F:a,b", &["-F:a,b"])]
#[case::spaced_long_flag("x --a = b", &["--a", "=", "b"])]
#[case::cut_fields("cut -d, -f1,3 f", &["-d,", "-f1,3", "f"])]
#[case::plus_mode("chmod +x file", &["+x", "file"])]
#[case::plus_echo("echo +x +rw", &["+x", "+rw"])]
#[case::symbolic_modes("chmod u+x,go-w,a=r f", &["u+x,go-w,a=r", "f"])]
#[case::ellipsis("echo ...", &["..."])]
#[case::ellipsis_between("echo a ... b", &["a", "...", "b"])]
#[case::after_double_dash("x -- -std=c11 +x ...", &["--", "-std=c11", "+x", "..."])]
fn flag_shaped_word_is_one_argument(#[case] source: &str, #[case] expected: &[&str]) {
    assert_eq!(planned_args(source), expected, "{source}");
}

/// A double-quoted value keeps its interpolation inside the one word.
#[test]
fn short_flag_with_interpolated_value_is_one_word() {
    let args = planned_args(r#"x -sOutputFile="$out/page.png""#);
    assert_eq!(args.len(), 1, "{args:?}");
    assert!(args[0].starts_with("plain:"), "{args:?}");
    assert!(args[0].contains("-sOutputFile="), "{args:?}");
}

/// Spaces around `=` keep three words, as before: `cut -d = -f2` and
/// `cut -d= -f2` pass `=` as the delimiter.
#[rstest]
#[case::spaced("x -a = b", &["-a", "=", "b"])]
#[case::delimiter_equals("cut -d= -f2", &["-d", "=", "-f2"])]
fn spaced_equals_is_not_fused(#[case] source: &str, #[case] expected: &[&str]) {
    assert_eq!(planned_args(source), expected, "{source}");
}

/// Text glued to a substitution, or to a quoted fragment, is still pasting.
#[rstest]
#[case::var_after_value("x -a=b$y")]
#[case::text_after_var("x -a=$y.txt")]
#[case::colon_list_var("awk -F:$y f")]
#[case::quoted_then_bare(r#"x -a="b"c"#)]
#[case::bare_then_quoted(r#"x -a=b"c""#)]
#[case::second_equals("x -a=b=c")]
#[case::linker_var("gcc -Wl,-rpath,$dir main.o")]
#[case::linker_quoted(r#"gcc -Wl,-rpath,"$dir" main.o"#)]
#[case::plus_var("echo +x$y")]
#[case::ellipsis_var("echo ...$y")]
fn substitution_glued_to_text_is_refused(#[case] source: &str) {
    let message = parse_error(source);
    assert!(message.contains("quote the whole word"), "{source}: {message}");
}

/// Spread is unchanged inside a list literal.
#[tokio::test]
async fn spread_in_list_literal_is_unchanged() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "a=[1,2]; b=[...$a 3]; echo ${#b}").await;
    assert_eq!((out.as_str(), code), ("3", 0));
}

#[tokio::test]
async fn echo_prints_flag_shaped_words() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) =
        run(&kernel, r#"y=1; echo -std=c11 -a="$y z" +x ... -Dk='v w'"#).await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out, "-std=c11 -a=1 z +x ... -Dk=v w");
}

#[tokio::test]
async fn arithmetic_and_set_plus_are_unchanged() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "set -e; set +e; a=2; echo $(( a + 3 )) $(( +4 ))").await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out, "5 4");
}

/// The glued comma idiom reaches the builtin binder as one value.
#[tokio::test]
async fn cut_and_sort_take_comma_lists_glued_to_the_flag() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    std::fs::write(tmp.path().join("d.csv"), "b,2,x\na,10,y\nc,1,z\n").unwrap();

    let (out, code) = run(&kernel, "cut -d, -f1,3 d.csv").await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out, "b,x\na,y\nc,z");

    let (out, code) = run(&kernel, "sort -t, -k2,2n d.csv").await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out, "c,1,z\nb,2,x\na,10,y");
}

/// A comma list on a builtin's bool flag is refused, never bound as flags
/// named `,` or `é`.
#[tokio::test]
async fn comma_list_on_a_bool_flag_is_refused() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    for (source, separator) in
        [("ls -l,a", ","), ("ls -l,é", ","), ("set -e,", ","), ("set -o,trash", ","), ("ls -l:a", ":")]
    {
        let result = kernel.execute(source).await.expect("execute");
        assert_ne!(result.code, 0, "{source} must fail: {}", result.text_out());
        assert!(
            result.err.contains(&format!("no flag before `{separator}` takes a value")),
            "{source}: {}",
            result.err
        );
    }
    // A value flag before the comma owns the list: `sort -rk2,2n`.
    std::fs::write(tmp.path().join("d.txt"), "a 1\nb 2\n").unwrap();
    let (out, code) = run(&kernel, "sort -rk2,2n d.txt").await;
    assert_eq!((out.as_str(), code), ("b 2\na 1", 0));
}

/// An external command receives each flag-shaped word as one argv entry.
#[cfg(feature = "subprocess")]
#[tokio::test]
async fn external_argv_keeps_flag_shaped_words_whole() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let result = kernel
        .execute(r#"/usr/bin/printf '<%s>' -std=c11 -interaction="non stop" -Wl,-rpath,/x -Wl,-rpath=/y -k2,2n +x ..."#)
        .await
        .expect("execute");
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(
        result.text_out(),
        "<-std=c11><-interaction=non stop><-Wl,-rpath,/x><-Wl,-rpath=/y><-k2,2n><+x><...>"
    );
}

/// The value slot of `-name=` follows the rules of `--name=`: the same
/// substitutions, the same rendering, the same refusals.
#[rstest]
#[case::variable("$y")]
#[case::braced_variable("${y}")]
#[case::command_substitution("$(echo b)")]
#[case::arithmetic("$((1 + 2))")]
#[case::tilde("~/f")]
#[case::bare_tilde("~")]
#[case::glob("*.c")]
#[case::double_quoted(r#""$y z""#)]
#[case::literal("b")]
fn short_flag_value_slot_matches_long_flag(#[case] value: &str) {
    for after_dash in ["", "-- "] {
        let short = planned_args(&format!("x {after_dash}-a={value}"));
        let long = planned_args(&format!("x {after_dash}--a={value}"));
        let long_as_short: Vec<String> = long.iter().map(|word| word.replacen("--a=", "-a=", 1)).collect();
        assert_eq!(short, long_as_short, "value {value:?} after {after_dash:?}");
    }
}

#[tokio::test]
async fn short_flag_value_slot_expands() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(
        &kernel,
        "HOME=/h; y=v; printf '<%s>' -a=$y -b=${y} -c=$(echo s) -d=$((1 + 2)) -e=~/f -- -f=$y",
    )
    .await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out, "<-a=v><-b=v><-c=s><-d=3><-e=/h/f><-f=v>");
}

/// A builtin reads `-n=5` as an operand and names the word in its error.
#[tokio::test]
async fn builtin_error_names_the_flag_value_word() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let result = kernel.execute("head -n=5 missing.txt").await.expect("execute");
    assert_ne!(result.code, 0);
    assert!(result.err.contains("-n=5"), "{}", result.err);
}

/// A list or record in the value slot is refused for an external command,
/// as `--a=$x` is.
#[cfg(feature = "subprocess")]
#[tokio::test]
async fn external_short_flag_value_refuses_a_list_like_the_long_flag() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let long = kernel.execute("x=[1,2]; /usr/bin/printf '%s' --a=$x").await.expect("execute");
    let short = kernel.execute("x=[1,2]; /usr/bin/printf '%s' -a=$x").await.expect("execute");
    assert_ne!(long.code, 0, "{}", long.text_out());
    assert_eq!((short.code, short.err.clone()), (long.code, long.err.clone()));
}

#[cfg(feature = "subprocess")]
#[tokio::test]
async fn external_argv_gets_expanded_short_flag_values() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let result = kernel
        .execute("HOME=/h; d=/opt/lib; /usr/bin/printf '<%s>' -Wl,-rpath=$d -I=~/inc -F:a --a = b")
        .await
        .expect("execute");
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(result.text_out(), "<-Wl,-rpath=/opt/lib><-I=/h/inc><-F:a><--a><=><b>");
}

/// A value flag takes a following `-name=value` word as its value, as it
/// takes any operand: `seq -s -n=5` separates with `-n=5`.
#[tokio::test]
async fn value_flag_takes_a_short_flag_value_word() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "seq -s -n=5 1 3; echo; seq -s -e=1.50 1 2").await;
    assert_eq!((out.as_str(), code), ("1-n=52-n=53\n1-e=1.502", 0));
}
