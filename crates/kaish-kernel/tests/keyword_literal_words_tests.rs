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

// `merge_colon_adjacent` (lexer.rs) fuses a colon-adjacent run into one
// plain `Ident` before the parser ever sees a `TildePath` token, so `~/a:b`
// as a bare word never reaches tilde expansion — same pre-existing,
// colon-adjacent gap as `tilde_expansion_tests.rs`'s
// `colon_adjacent_tilde_in_assignment_value_is_unsupported` (kaish has
// never supported a `:`-adjacent `~`, unlike bash). This test used to
// assert expansion here, but that only passed because of the bug tracked
// by `tilde_expansion_tests.rs`: the pre-fix kernel expanded ANY evaluated
// string starting with `~`, quoted or not, so this bare (unquoted) case
// happened to come out right by the wrong mechanism — one that would also
// have wrongly expanded a quoted `'~/a:b'`.
#[tokio::test]
async fn colon_tilde_path_is_literal_and_keyword_glob_matches_files() {
    let kernel = Kernel::new(KernelConfig::isolated().with_initial_vars(
        [("HOME".into(), Value::String("/home/fixture".into()))].into())).unwrap();
    let result = kernel.execute("echo ~/a:b; touch /true-one /true-two /other; cd /; echo true*").await.unwrap();
    assert!(result.ok(), "{result:?}");
    assert_eq!(result.text_out(), "~/a:b\ntrue-one true-two\n");
}
