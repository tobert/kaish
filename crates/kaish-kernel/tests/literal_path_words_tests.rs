//! Literal paths keep their source spelling across lexer and parser contexts.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::ast::{Arg, Command, Expr, Stmt, Value};
use kaish_kernel::lexer::{Token, tokenize};
use kaish_kernel::parser::parse;
use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

#[rstest]
#[case(".git/HEAD")]
#[case(".git/logs/HEAD")]
#[case(".git/")]
#[case("./")]
#[case("../")]
#[case("2026/report")]
#[case("007/0644")]
#[case("1.0/report")]
#[case(".5/report")]
#[case(".日本/設定")]
#[case("a+b/file")]
#[case("a@b/file")]
#[case("/tmp/a@b")]
#[case("@scope/pkg+tag")]
#[case("repo/.git/HEAD")]
#[case("./.git/HEAD")]
#[case("../HEAD")]
#[case("foo#bar/file")]
fn literal_path_is_one_word_in_each_context(#[case] path: &str) {
    let tokens = tokenize(path).unwrap();
    assert_eq!(tokens.len(), 1, "{path}: {tokens:?}");
    assert_eq!(tokens[0].span, 0..path.len());
    let text = match &tokens[0].token {
        Token::Path(text)
        | Token::RelativePath(text)
        | Token::DotSlashPath(text)
        | Token::TildePath(text)
        | Token::AtWord(text) => text,
        other => panic!("expected literal path: {other:?}"),
    };
    assert_eq!(text, path);

    for source in [format!("echo {path}"), format!("echo -- {path}")] {
        let program = parse(&source).unwrap();
        assert_eq!(program.statements.len(), 1);
        let command = only_command(&program.statements);
        let arguments: Vec<_> = command
            .args
            .iter()
            .filter(|arg| !matches!(arg, Arg::DoubleDash))
            .collect();
        assert_eq!(arguments.len(), 1, "{source}: {arguments:?}");
        assert_eq!(
            arguments[0],
            &Arg::Positional(Expr::Literal(Value::String(path.into())))
        );
    }

    let program = parse(&format!("p={path}")).unwrap();
    assert_eq!(program.statements.len(), 1);
    let Stmt::Assignment(assignment) = &program.statements[0] else {
        panic!("{program:?}")
    };
    assert_eq!(assignment.value, Expr::Literal(Value::String(path.into())));

    let program = parse(&format!("cat <{path}")).unwrap();
    assert_eq!(program.statements.len(), 1);
    let command = only_command(&program.statements);
    assert!(command.args.is_empty());
    assert_eq!(command.redirects.len(), 1);
    assert_eq!(
        command.redirects[0].target,
        Expr::Literal(Value::String(path.into()))
    );
}

#[rstest]
#[case("HEAD:src/main.rs")]
#[case("HEAD:.git/config")]
#[case("host:./dir/file")]
#[case("host:../dir/file")]
#[case("HEAD:007/0644")]
#[case("http://host/path")]
fn colon_path_keeps_one_literal_argument(#[case] path: &str) {
    let program = parse(&format!("echo {path}")).unwrap();
    assert_eq!(program.statements.len(), 1);
    let command = only_command(&program.statements);
    assert_eq!(
        command.args,
        vec![Arg::Positional(Expr::Literal(Value::String(path.into())))]
    );
}

#[rstest]
#[case(".git/$name")]
#[case("2026/$(echo report)")]
#[case(".git/\"HEAD\"")]
#[case(".git/'HEAD'")]
#[case("$dir/HEAD")]
#[case(".git/$((1+2))")]
fn path_fragments_still_require_quoting(#[case] word: &str) {
    for source in [
        format!("echo {word}"),
        format!("echo -- {word}"),
        format!("cat <{word}"),
    ] {
        let errors = parse(&source).expect_err(&source);
        assert!(
            errors.iter().any(|error| error.message.contains("quote")),
            "{source}: {errors:?}"
        );
    }
}

#[rstest]
#[case(".git/*")]
#[case("2026/*.txt")]
#[case("1.0/file*")]
#[case(".git/[ab]")]
fn path_globs_remain_patterns(#[case] pattern: &str) {
    let program = parse(&format!("echo {pattern}")).unwrap();
    let command = only_command(&program.statements);
    assert_eq!(
        command.args,
        vec![Arg::Positional(Expr::GlobPattern(pattern.into()))]
    );
}

#[tokio::test]
async fn reported_command_reads_the_intended_files() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    let setup = kernel.execute(r#"mkdir -p ".git/logs"; echo ref:main >".git/HEAD"; printf 'User-Agent one\nother\nuser-agent two\n' >".git/logs/HEAD""#).await.unwrap();
    assert_eq!(setup.code, 0, "{}", setup.err);
    let result = kernel.execute(r#"cat .git/HEAD; grep -c "" .git/logs/HEAD; grep -n "User-Agent\|user-agent" .git/logs/HEAD | tail -n 5"#).await.unwrap();
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(
        result.text_out(),
        "ref:main\n3\n1:User-Agent one\n3:user-agent two\n"
    );
}

fn only_command(statements: &[Stmt]) -> &Command {
    assert_eq!(statements.len(), 1);
    match &statements[0] {
        Stmt::Command(command) => command,
        Stmt::Pipeline(pipeline) if pipeline.stages.len() == 1 => {
            pipeline.stages[0].as_command().unwrap()
        }
        other => panic!("expected one command: {other:?}"),
    }
}

#[rstest]
#[case(".git/hooks/pre-commit")]
#[case("src/bin")]
#[case("../bin")]
#[case("2026/bin")]
#[case("./bin")]
#[case("/tmp/bin")]
fn literal_executable_paths_keep_the_command_name(#[case] path: &str) {
    let program = parse(&format!("{path} argument")).unwrap();
    let command = only_command(&program.statements);
    assert_eq!(command.name, path);
    assert_eq!(
        command.args,
        vec![Arg::Positional(Expr::Literal(Value::String(
            "argument".into()
        )))]
    );
}

#[rstest]
#[case("./bin$x")]
#[case("/tmp/bin$x")]
#[case(".git/hooks/$name")]
#[case("src/$(echo bin)")]
#[case("./bin\"suffix\"")]
#[case("./bin' suffix'")]
fn executable_fragments_do_not_become_arguments(#[case] source: &str) {
    assert!(
        parse(source).is_err(),
        "{source} must not run a different executable"
    );
}

#[tokio::test]
async fn collections_numbers_and_source_keep_their_meaning() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    let result = kernel.execute(r#"p=.git/HEAD; xs=[.git/HEAD 007/0644]; r={file:.git/HEAD}; echo "$p"; echo "${xs[1]}"; echo "${r[file]}"; echo $((6 / 2)); echo 007; echo 1.0; echo 'echo sourced' > script; . script"#).await.unwrap();
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(
        result.text_out(),
        ".git/HEAD\n007/0644\n.git/HEAD\n3\n007\n1.0\nsourced\n"
    );
}

#[tokio::test]
async fn path_words_reach_the_tool_as_exact_arguments() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    for path in [
        ".git/HEAD",
        "007/0644",
        "a+b/file",
        "a@b/file",
        "HEAD:src/main.rs",
    ] {
        let result = kernel
            .execute(&format!("printf '%s\\n' before {path} after"))
            .await
            .unwrap();
        assert_eq!(result.code, 0, "{}", result.err);
        assert_eq!(result.text_out(), format!("before\n{path}\nafter\n"));
    }
}

#[rstest]
#[case("./bin>out")]
#[case("./bin|cat")]
#[case("./bin;echo next")]
#[case("./bin&&echo next")]
#[case("./bin argument")]
#[case(". script")]
fn command_operators_and_spaced_arguments_still_parse(#[case] source: &str) {
    assert!(parse(source).is_ok(), "{source}");
}

#[test]
fn tilde_path_keeps_at_sign_in_arguments_and_redirects() {
    let tokens = tokenize("~/a@b").unwrap();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].token, Token::TildePath("~/a@b".into()));
    let program = parse("cat ~/a@b").unwrap();
    assert_eq!(
        only_command(&program.statements).args,
        vec![Arg::Positional(Expr::Literal(Value::String(
            "~/a@b".into()
        )))]
    );
    let program = parse("cat <~/a@b").unwrap();
    assert_eq!(
        only_command(&program.statements).redirects[0].target,
        Expr::Literal(Value::String("~/a@b".into()))
    );
}
