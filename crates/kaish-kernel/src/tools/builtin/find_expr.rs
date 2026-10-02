//! find's expression grammar: tests, `!`, `-a`, `-o`, and `( )`.
//!
//! Precedence follows GNU find: `!` binds tighter than `-a` (also the
//! implicit joiner between two tests), which binds tighter than `-o`.
//! `-maxdepth` and `-mindepth` are options that read as an always-true test;
//! they apply to the whole walk wherever they appear.

use kaish_glob::glob_match;

use crate::vfs::DirEntry;

const MAX_EXPRESSION_NODES: usize = 256;
const MAX_EXPRESSION_NESTING: usize = 64;

/// A node in the parsed expression.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum Expr {
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    Test(Test),
    /// `-print`: true, and marks the entry for output.
    Print,
    /// An option, or an empty expression: always true.
    True,
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Test {
    /// `-name` / `-iname`: glob against the last path component.
    Name { pattern: String, fold_case: bool },
    /// `-path` / `-ipath`: glob against the whole path as printed.
    Path { pattern: String, fold_case: bool },
    /// `-type f|d|l`.
    Type(char),
    /// `-mtime [+-]N`, in days.
    Mtime(Comparison),
    /// `-size [+-]N[KMG]`, in bytes.
    Size(Comparison),
}

/// `+N` greater than, `-N` less than, `N` equal.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) struct Comparison {
    pub sign: char,
    pub amount: u64,
}

impl Comparison {
    fn holds(self, actual: u64) -> bool {
        match self.sign {
            '+' => actual > self.amount,
            '-' => actual < self.amount,
            _ => actual == self.amount,
        }
    }
}

/// Walk limits read from `-maxdepth` / `-mindepth`.
#[derive(Debug, Default, PartialEq)]
pub(super) struct DepthOptions {
    pub max: Option<usize>,
    pub min: Option<usize>,
}

#[derive(Debug, PartialEq)]
pub(super) struct Parsed {
    pub expr: Expr,
    pub depth: DepthOptions,
}

/// Starting paths are the leading words that cannot begin an expression.
pub(super) fn split_operands(words: &[String]) -> (&[String], &[String]) {
    let end = words
        .iter()
        .position(|w| (w.starts_with('-') && w != "-") || matches!(w.as_str(), "!" | "(" | ")"))
        .unwrap_or(words.len());
    words.split_at(end)
}

/// Parse the expression words that follow the starting paths.
///
/// Errors are the complete text after `find: `.
pub(super) fn parse(words: &[String]) -> Result<Parsed, String> {
    let mut parser = Parser {
        words, at: 0, depth: DepthOptions::default(),
        nodes_remaining: MAX_EXPRESSION_NODES, nesting: 0,
    };
    if words.is_empty() {
        return Ok(Parsed { expr: Expr::True, depth: parser.depth });
    }
    let expr = parser.or_expression()?;
    if let Some(word) = parser.peek() {
        return Err(if word == ")" {
            "')' has no matching '('".to_string()
        } else {
            format!("'{word}' is not an operator or test here")
        });
    }
    Ok(Parsed { expr, depth: parser.depth })
}

struct Parser<'a> {
    words: &'a [String],
    at: usize,
    depth: DepthOptions,
    nodes_remaining: usize,
    nesting: usize,
}

impl Parser<'_> {
    fn charge_node(&mut self) -> Result<(), String> {
        self.nodes_remaining = self.nodes_remaining.checked_sub(1).ok_or_else(|| {
            format!("expression has more than {MAX_EXPRESSION_NODES} tests, operators, or groups; use a smaller expression")
        })?;
        Ok(())
    }

    fn nested(&mut self, parse: fn(&mut Self) -> Result<Expr, String>) -> Result<Expr, String> {
        if self.nesting >= MAX_EXPRESSION_NESTING {
            return Err(format!("expression nesting exceeds {MAX_EXPRESSION_NESTING}; use fewer groups or ! operators"));
        }
        self.nesting += 1;
        let result = parse(self);
        self.nesting -= 1;
        result
    }

    fn peek(&self) -> Option<&str> {
        self.words.get(self.at).map(String::as_str)
    }

    fn advance(&mut self) -> Option<&str> {
        let word = self.words.get(self.at).map(String::as_str);
        if word.is_some() {
            self.at += 1;
        }
        word
    }

    fn or_expression(&mut self) -> Result<Expr, String> {
        let mut left = self.and_expression()?;
        while let Some(word @ ("-o" | "-or")) = self.peek() {
            let word = word.to_string();
            self.at += 1;
            if self.at >= self.words.len() || self.peek() == Some(")") {
                return Err(format!("{word} needs a test after it"));
            }
            self.charge_node()?;
            let right = self.and_expression()?;
            left = Expr::Or(Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn and_expression(&mut self) -> Result<Expr, String> {
        let mut left = self.not_expression()?;
        loop {
            match self.peek() {
                None | Some("-o" | "-or" | ")") => return Ok(left),
                Some(word @ ("-a" | "-and")) => {
                    let word = word.to_string();
                    self.at += 1;
                    if self.at >= self.words.len() || self.peek() == Some(")") {
                        return Err(format!("{word} needs a test after it"));
                    }
                }
                Some(_) => {} // two tests side by side are joined by -a
            }
            self.charge_node()?;
            let right = self.not_expression()?;
            left = Expr::And(Box::new(left), Box::new(right));
        }
    }

    fn not_expression(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(word @ ("!" | "-not")) => {
                let word = word.to_string();
                self.at += 1;
                if self.at >= self.words.len() || self.peek() == Some(")") {
                    return Err(format!("{word} needs a test after it"));
                }
                self.charge_node()?;
                Ok(Expr::Not(Box::new(self.nested(Self::not_expression)?)))
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> Result<Expr, String> {
        self.charge_node()?;
        let Some(word) = self.advance().map(str::to_string) else {
            return Err("the expression ends where a test was expected".to_string());
        };
        match word.as_str() {
            "(" => {
                if self.peek() == Some(")") {
                    return Err("'(' ')' has no test inside".to_string());
                }
                let inner = self.nested(Self::or_expression)?;
                match self.advance() {
                    Some(")") => Ok(inner),
                    _ => Err("'(' has no matching ')'".to_string()),
                }
            }
            "-o" | "-or" | "-a" | "-and" => Err(format!("{word} needs a test before it")),
            _ => self.test(&word),
        }
    }

    /// One test or option. `--name` and `--name=VALUE` read as `-name`.
    fn test(&mut self, word: &str) -> Result<Expr, String> {
        let (name, inline) = match word.strip_prefix("--") {
            Some(long) if !long.is_empty() => match long.split_once('=') {
                Some((key, value)) => (format!("-{key}"), Some(value.to_string())),
                None => (format!("-{long}"), None),
            },
            _ => (word.to_string(), None),
        };
        let value = |parser: &mut Self| -> Result<String, String> {
            if let Some(v) = inline.clone() {
                return Ok(v);
            }
            match parser.advance() {
                Some(v) => Ok(v.to_string()),
                None => Err(format!("{name} needs a value")),
            }
        };
        match name.as_str() {
            "-name" | "-iname" => Ok(Expr::Test(Test::Name {
                pattern: value(self)?,
                fold_case: name == "-iname",
            })),
            "-path" | "-wholename" | "-ipath" => Ok(Expr::Test(Test::Path {
                pattern: value(self)?,
                fold_case: name == "-ipath",
            })),
            "-type" => {
                let v = value(self)?;
                match v.as_str() {
                    "f" => Ok(Expr::Test(Test::Type('f'))),
                    "d" => Ok(Expr::Test(Test::Type('d'))),
                    "l" => Ok(Expr::Test(Test::Type('l'))),
                    _ => Err(format!("invalid type '{v}': use 'f', 'd', or 'l'")),
                }
            }
            "-mtime" => {
                let v = value(self)?;
                parse_comparison(&v, false)
                    .map(|c| Expr::Test(Test::Mtime(c)))
                    .ok_or_else(|| format!("invalid -mtime '{v}': expected N, +N, or -N days"))
            }
            "-size" => {
                let v = value(self)?;
                parse_comparison(&v, true)
                    .map(|c| Expr::Test(Test::Size(c)))
                    .ok_or_else(|| {
                        format!("invalid -size '{v}': expected N, +N, or -N bytes, with an optional K, M, or G")
                    })
            }
            "-maxdepth" | "-mindepth" => {
                let v = value(self)?;
                let depth: usize = v.parse().map_err(|_| {
                    format!("invalid {name} '{v}': expected a non-negative integer")
                })?;
                if name == "-maxdepth" {
                    self.depth.max = Some(depth);
                } else {
                    self.depth.min = Some(depth);
                }
                Ok(Expr::True)
            }
            "-print" if inline.is_some() => Err(format!("{word} does not take a value; use -print")),
            "-print" => Ok(Expr::Print),
            _ => Err(format!("{word} is not supported (see `help find`)")),
        }
    }
}

/// Parse `+7`, `-3`, `5`; with `units`, also `10K`, `+2M`, `1G`.
fn parse_comparison(text: &str, units: bool) -> Option<Comparison> {
    let (sign, rest) = match text.chars().next()? {
        sign @ ('+' | '-') => (sign, &text[1..]),
        _ => ('=', text),
    };
    let (digits, multiplier) = match rest.chars().last()?.to_ascii_uppercase() {
        'K' if units => (&rest[..rest.len() - 1], 1024u64),
        'M' if units => (&rest[..rest.len() - 1], 1024 * 1024),
        'G' if units => (&rest[..rest.len() - 1], 1024 * 1024 * 1024),
        _ => (rest, 1),
    };
    let amount = digits.parse::<u64>().ok()?.checked_mul(multiplier)?;
    Some(Comparison { sign, amount })
}

/// What a test reads about one walked entry.
pub(super) struct EntryView<'a> {
    /// The path as find prints it.
    pub display: &'a str,
    pub info: Option<&'a DirEntry>,
}

impl EntryView<'_> {
    fn name(&self) -> &str {
        std::path::Path::new(self.display)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or(self.display)
    }
}

/// True when `expr` holds for `entry`. Sets `printed` when a `-print` ran.
pub(super) fn evaluate(expr: &Expr, entry: &EntryView<'_>, printed: &mut usize) -> bool {
    match expr {
        Expr::True => true,
        Expr::Print => {
            *printed += 1;
            true
        }
        Expr::Not(inner) => !evaluate(inner, entry, printed),
        Expr::And(left, right) => {
            evaluate(left, entry, printed) && evaluate(right, entry, printed)
        }
        Expr::Or(left, right) => {
            evaluate(left, entry, printed) || evaluate(right, entry, printed)
        }
        Expr::Test(test) => test_holds(test, entry),
    }
}

/// Count evaluated `-print` actions, or one implicit print for a true expression.
pub(super) fn print_count(parsed: &Parsed, entry: &EntryView<'_>) -> usize {
    let mut printed = 0;
    let holds = evaluate(&parsed.expr, entry, &mut printed);
    if has_print(&parsed.expr) { printed } else { usize::from(holds) }
}

fn has_print(expr: &Expr) -> bool {
    match expr {
        Expr::Print => true,
        Expr::Not(inner) => has_print(inner),
        Expr::And(l, r) | Expr::Or(l, r) => has_print(l) || has_print(r),
        Expr::Test(_) | Expr::True => false,
    }
}

fn test_holds(test: &Test, entry: &EntryView<'_>) -> bool {
    match test {
        Test::Name { pattern, fold_case } => glob_text(pattern, entry.name(), *fold_case),
        Test::Path { pattern, fold_case } => glob_text(pattern, entry.display, *fold_case),
        Test::Type(kind) => entry.info.is_some_and(|i| match kind {
            'f' => i.is_file(),
            'd' => i.is_dir(),
            _ => i.is_symlink(),
        }),
        // A test that needs stat data is false when the entry has none.
        Test::Mtime(cmp) => match entry.info.and_then(|i| i.modified) {
            Some(modified) => {
                let age_days = modified.elapsed().map(|d| d.as_secs()).unwrap_or(0) / 86400;
                cmp.holds(age_days)
            }
            None => false,
        },
        Test::Size(cmp) => entry.info.is_some_and(|i| cmp.holds(i.size)),
    }
}

fn glob_text(pattern: &str, text: &str, fold_case: bool) -> bool {
    if fold_case {
        glob_match(&pattern.to_lowercase(), &text.to_lowercase())
    } else {
        glob_match(pattern, text)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn words(text: &str) -> Vec<String> {
        text.split_whitespace().map(str::to_string).collect()
    }

    fn name(pattern: &str) -> Expr {
        Expr::Test(Test::Name { pattern: pattern.into(), fold_case: false })
    }

    fn and(l: Expr, r: Expr) -> Expr {
        Expr::And(Box::new(l), Box::new(r))
    }

    fn or(l: Expr, r: Expr) -> Expr {
        Expr::Or(Box::new(l), Box::new(r))
    }

    #[test]
    fn and_binds_tighter_than_or() {
        let parsed = parse(&words("-name a -o -name b -a -name c")).unwrap();
        assert_eq!(parsed.expr, or(name("a"), and(name("b"), name("c"))));
    }

    #[test]
    fn adjacent_tests_are_anded() {
        let parsed = parse(&words("-name a -name b")).unwrap();
        assert_eq!(parsed.expr, and(name("a"), name("b")));
    }

    #[test]
    fn not_binds_tighter_than_and() {
        let parsed = parse(&words("! -name a -name b")).unwrap();
        assert_eq!(parsed.expr, and(Expr::Not(Box::new(name("a"))), name("b")));
    }

    #[test]
    fn group_overrides_precedence() {
        let parsed = parse(&words("( -name a -o -name b ) -name c")).unwrap();
        assert_eq!(parsed.expr, and(or(name("a"), name("b")), name("c")));
    }

    #[test]
    fn depth_options_are_collected_and_read_as_true() {
        let parsed = parse(&words("-maxdepth 2 -mindepth 1 -name a")).unwrap();
        assert_eq!(parsed.depth, DepthOptions { max: Some(2), min: Some(1) });
        assert_eq!(parsed.expr, and(and(Expr::True, Expr::True), name("a")));
    }

    #[test]
    fn long_spelling_is_accepted() {
        let parsed = parse(&words("--name=a --type f")).unwrap();
        assert_eq!(parsed.expr, and(name("a"), Expr::Test(Test::Type('f'))));
    }

    #[test]
    fn operands_stop_at_the_first_expression_word() {
        let all = words("a b -name x");
        let (paths, rest) = split_operands(&all);
        assert_eq!((paths.len(), rest.len()), (2, 2));
        let all = words("a ( -name x )");
        assert_eq!(split_operands(&all).0.len(), 1);
    }

    #[test]
    fn comparison_parses_units() {
        assert_eq!(parse_comparison("+1K", true), Some(Comparison { sign: '+', amount: 1024 }));
        assert_eq!(parse_comparison("-5", false), Some(Comparison { sign: '-', amount: 5 }));
        assert_eq!(parse_comparison("2G", true).map(|c| c.amount), Some(2 * 1024 * 1024 * 1024));
        assert_eq!(parse_comparison("1K", false), None);
        assert_eq!(parse_comparison("x", true), None);
    }

    #[rstest::rstest]
    #[case::negation(format!("{}-name x", "! ".repeat(65)))]
    #[case::groups(format!("{}-name x{}", "( ".repeat(65), " )".repeat(65)))]
    #[case::chain(std::iter::repeat_n("-name x", 130).collect::<Vec<_>>().join(" "))]
    #[test]
    fn excessive_expression_depth_or_size_is_refused(#[case] source: String) {
        assert!(parse(&words(&source)).is_err(), "unbounded expression accepted");
    }

    #[test]
    fn expressions_at_the_nesting_and_node_limits_are_accepted() {
        assert!(parse(&words(&format!("{}-name x", "! ".repeat(64)))).is_ok());
        let chain = std::iter::repeat_n("-name x", 128).collect::<Vec<_>>().join(" ");
        assert!(parse(&words(&chain)).is_ok());
    }

    #[test]
    fn print_refuses_an_attached_value() {
        assert!(parse(&words("--print=ignored")).is_err());
    }

    #[test]
    fn errors_name_the_problem() {
        let message = |text: &str| parse(&words(text)).unwrap_err();
        assert!(message("( -name a").contains("no matching ')'"));
        assert!(message("-name a )").contains("no matching '('"));
        assert!(message("-name a -o").contains("-o needs a test after it"));
        assert!(message("-o -name a").contains("-o needs a test before it"));
        assert!(message("-name").contains("-name needs a value"));
        assert!(message("-delete").contains("-delete is not supported"));
        assert!(message("( )").contains("no test inside"));
    }
}
