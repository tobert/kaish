//! Numeric prefixes do not turn a literal filename into a malformed number.
#![allow(clippy::unwrap_used, clippy::expect_used)]
use kaish_kernel::ast::{Arg, Expr, PipelineStage, Stmt, Value};
use kaish_kernel::lexer::{tokenize, NumericLiteralData, Token};
use kaish_kernel::parser::parse;
use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

#[rstest]
#[case("123.txt")]
#[case("123+b")]
#[case("123@host")]
#[case("1.0+2")]
#[case("1.2.3")]
#[case("1.0.txt")]
#[case("007.0644.txt")]
#[case("1.2rc1")]
#[case("1.2.3-rc1")]
#[case("123.日本語")]
#[case("9223372036854775808.txt")]
#[case(".123.txt")]
fn numeric_prefixed_word_keeps_its_exact_text(#[case] word: &str) {
    let tokens = tokenize(word).unwrap();
    assert_eq!(tokens.len(), 1, "{word}: {tokens:?}");
    assert_eq!(tokens[0].span, 0..word.len());
    let program = parse(&format!("echo {word}")).unwrap();
    let Stmt::Command(command) = &program.statements[0] else { panic!("{program:?}") };
    assert_eq!(command.args, vec![Arg::Positional(Expr::Literal(Value::String(word.into())))]);
    let program = parse(&format!("p={word}")).unwrap();
    let Stmt::Assignment(assignment) = &program.statements[0] else { panic!("{program:?}") };
    assert_eq!(assignment.value, Expr::Literal(Value::String(word.into())));
    let program = parse(&format!("cat <{word}")).unwrap();
    let Stmt::Pipeline(pipeline) = &program.statements[0] else { panic!("{program:?}") };
    let PipelineStage::Command(command) = &pipeline.stages[0] else { panic!("{pipeline:?}") };
    assert_eq!(command.redirects[0].target, Expr::Literal(Value::String(word.into())));
}

#[rstest]
#[case("1.0*")]
#[case("007.00*")]
#[case("1.0?")]
#[case("1.0[ab]")]
fn float_prefixed_glob_keeps_its_source(#[case] word: &str) {
    let tokens = tokenize(word).unwrap();
    assert_eq!(tokens.len(), 1, "{tokens:?}");
    assert_eq!(tokens[0].token, Token::GlobWord(word.into()));
    assert_eq!(tokens[0].span, 0..word.len());
    parse(&format!("echo {word}")).unwrap();
}

#[tokio::test]
async fn glob_matches_the_written_prefix_and_quotes_keep_it_literal() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    let result = kernel.execute("touch /1.0a /1.0b /1a /007.00a /7a; cd /").await.unwrap();
    assert!(result.ok(), "{result:?}");
    for (source, expected) in [("echo 1.0*", "1.0a 1.0b\n"), ("echo 007.00*", "007.00a\n"), ("echo \"1.0*\"", "1.0*\n")] {
        let result = kernel.execute(source).await.unwrap();
        assert!(result.ok(), "{result:?}");
        assert_eq!(result.text_out(), expected);
    }
}

#[test]
fn scalar_number_contract_is_unchanged() {
    for word in [".5", "5.", "9223372036854775808"] {
        assert!(tokenize(word).is_err(), "{word} must remain invalid");
    }
    assert_eq!(tokenize("1.0").unwrap()[0].token, Token::NumericLiteral(NumericLiteralData {
        value: Value::Float(1.0), raw: "1.0".into(),
    }));
    assert_eq!(tokenize("123").unwrap()[0].token, Token::Int(123));
    assert_eq!(tokenize("1.25").unwrap()[0].token, Token::Float(1.25));
    assert_eq!(tokenize("007").unwrap()[0].token, Token::NumberIdent("007".into()));
    parse("x=[1.0,2.0]; y={version:1.2.3}").unwrap();
    assert!(parse("echo 1.2.3$tag").is_err());
}

#[rstest]
#[case("echo 123.txt$x")]
#[case("echo true:foo$x")]
#[case("echo a+b$x")]
#[case("echo 1.0*$x")]
#[case("for x in 123.txt$tag; do echo $x; done")]
#[case("cat <123.txt$x")]
fn literal_word_still_cannot_join_an_expansion(#[case] source: &str) {
    assert!(parse(source).is_err(), "{source}");
}

#[rstest]
#[case("123.txt", "123.txt")]
#[case("true:foo", "true:foo")]
#[case("+e", "+e")]
#[case("1.0a", "1.0*")]
#[tokio::test]
async fn literal_words_and_globs_work_as_case_patterns(#[case] word: &str, #[case] pattern: &str) {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    let result = kernel.execute(&format!("case '{word}' in {pattern}) echo match ;; *) echo miss ;; esac")).await.unwrap();
    assert!(result.ok(), "{result:?}");
    assert_eq!(result.text_out(), "match\n");
}
