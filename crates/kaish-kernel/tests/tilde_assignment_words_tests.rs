//! Assignment delimiters stay separate from home-relative paths.
#![allow(clippy::unwrap_used, clippy::expect_used)]
use kaish_kernel::ast::{Expr, Stmt, Value};
use kaish_kernel::lexer::{tokenize, Token};
use kaish_kernel::parser::parse;
use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

#[rstest]
#[case("~/x")]
#[case("~/")]
#[case("~")]
#[case("~/日本語")]
#[case("~/a+b")]
#[case("~/a:b")]
#[case("~fixture-user/x")]
#[case("~007")]
#[case("~1.0")]
#[case("~10-20")]
#[case("~2024-01-02")]
fn tilde_assignment_has_an_assignment_delimiter(#[case] path: &str) {
    let source = format!("p={path}");
    let tokens = tokenize(&source).unwrap();
    assert_eq!(tokens.len(), 3, "{tokens:?}");
    assert_eq!(tokens[1].token, Token::Eq);
    assert_eq!(tokens[1].span, 1..2);
    assert_eq!(tokens[2].span, 2..source.len());
    let program = parse(&source).unwrap();
    let Stmt::Assignment(assignment) = &program.statements[0] else { panic!("{program:?}") };
    assert_eq!(assignment.value, Expr::Literal(Value::String(path.into())));
}

#[rstest]
#[case("p=~/x; echo $p", "/home/fixture/x\n")]
#[case("p=~; echo $p", "/home/fixture\n")]
#[case("echo p=~/x", "p=/home/fixture/x\n")]
#[case("p=~/; echo $p", "/home/fixture/\n")]
#[case("echo $(p=~; echo $p)", "/home/fixture\n")]
#[case("local p=~/x; echo $p", "/home/fixture/x\n")]
#[case("p={x:empty}; p[x]=~/x; echo ${p[x]}", "/home/fixture/x\n")]
#[case("echo $(p=~/x; echo $p)", "/home/fixture/x\n")]
#[case("[[ $(p=~/x; echo $p) =~ /x ]] && echo yes", "yes\n")]
#[tokio::test]
async fn assignments_expand_against_session_home(#[case] source: &str, #[case] expected: &str) {
    let kernel = Kernel::new(KernelConfig::isolated().with_initial_vars(
        [("HOME".into(), Value::String("/home/fixture".into()))].into())).unwrap();
    let result = kernel.execute(source).await.unwrap();
    assert!(result.ok(), "{result:?}");
    assert_eq!(result.text_out(), expected);
}

#[rstest]
#[case("[[ x=~/x ]]")]
#[case("[[ a == a && x=~/x ]]")]
#[case("[[ a == a || x=~/x ]]")]
#[case("[[ x =~ /x ]]")]
#[case("[[ $(echo x)=~/x ]]")]
fn regex_operator_remains_whole_inside_tests(#[case] source: &str) {
    let tokens = tokenize(source).unwrap();
    assert_eq!(tokens.iter().filter(|t| matches!(t.token, Token::Match)).count(), 1);
    parse(source).unwrap();
}

#[test]
fn tilde_assignment_does_not_join_expansions() {
    assert!(parse("p=~/$name").is_err());
    parse("p=\"$HOME/$name\"").unwrap();
    parse("foo --path=~/x").unwrap();
}

#[test]
fn glob_bracket_adjacency_does_not_execute_as_separate_arguments() {
    assert!(parse("echo [x]=~/y").is_err());
}

#[test]
fn glob_bracket_is_not_an_assignment_target() {
    let tokens = tokenize("echo [x]=~/y").unwrap();
    assert_eq!(tokens.iter().filter(|t| matches!(t.token, Token::Match)).count(), 1);
}

#[tokio::test]
async fn named_path_argument_expands_before_tool_binding() {
    let kernel = Kernel::new(KernelConfig::isolated().with_initial_vars(
        [("HOME".into(), Value::String("/home/fixture".into()))].into())).unwrap();
    let result = kernel.execute("mkdir -p /home/fixture; touch /home/fixture/x /home/fixture/y; find /home/fixture --path=~/x").await.unwrap();
    assert!(result.ok(), "{result:?}");
    assert_eq!(result.text_out(), "/home/fixture/x");
}
