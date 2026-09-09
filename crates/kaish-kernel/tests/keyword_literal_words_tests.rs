//! Reserved words remain reserved only when they occupy the whole word.
#![allow(clippy::unwrap_used, clippy::expect_used)]
use kaish_kernel::ast::{Arg, Expr, Stmt, Value};
use kaish_kernel::lexer::{tokenize, Token};
use kaish_kernel::parser::parse;
use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

#[rstest]
#[case("true:foo")]
#[case("false:foo")]
#[case("do:foo")]
#[case("for:foo")]
#[case("string:foo")]
#[case("~/a:b")]
#[case("a+b")]
#[case("a+b+c")]
#[case(".a+b")]
#[case("123a+b")]
fn literal_word_keeps_one_argument(#[case] word: &str) {
    let program = parse(&format!("echo {word}")).unwrap();
    let Stmt::Command(command) = &program.statements[0] else { panic!("{program:?}") };
    assert_eq!(command.args, vec![Arg::Positional(Expr::Literal(Value::String(word.into())))]);
}

#[rstest]
#[case("true*")]
#[case("do?")]
#[case("false[ab]")]
fn keyword_prefixed_pattern_is_a_glob(#[case] word: &str) {
    let tokens = tokenize(word).unwrap();
    assert_eq!(tokens.len(), 1, "{tokens:?}");
    assert_eq!(tokens[0].token, Token::GlobWord(word.into()));
    parse(&format!("echo {word}")).unwrap();
}

#[tokio::test]
async fn whole_keywords_records_and_plus_flags_keep_their_meaning() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    let result = kernel.execute("set +e; x={enabled:true,disabled:false}; if true; then for item in do:foo true:foo; do echo $item; done; fi; echo ${x[enabled]} ${x[disabled]}").await.unwrap();
    assert!(result.ok(), "{result:?}");
    assert_eq!(result.text_out(), "do:foo\ntrue:foo\ntrue false\n");
    assert_eq!(tokenize("+e").unwrap()[0].token, Token::PlusFlag("e".into()));
    assert_eq!(tokenize("true").unwrap()[0].token, Token::True);
}

#[tokio::test]
async fn colon_tilde_path_expands_home_and_keyword_glob_matches_files() {
    let kernel = Kernel::new(KernelConfig::isolated().with_initial_vars(
        [("HOME".into(), Value::String("/home/fixture".into()))].into())).unwrap();
    let result = kernel.execute("echo ~/a:b; touch /true-one /true-two /other; cd /; echo true*").await.unwrap();
    assert!(result.ok(), "{result:?}");
    assert_eq!(result.text_out(), "/home/fixture/a:b\ntrue-one true-two\n");
}
