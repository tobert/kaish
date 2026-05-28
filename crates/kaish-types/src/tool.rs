//! Tool schema and argument types.

use std::collections::{BTreeMap, HashSet};

use crate::value::Value;

fn default_consumes() -> usize {
    1
}

/// Schema for a tool parameter.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ParamSchema {
    /// Parameter name.
    pub name: String,
    /// Type hint (string, int, bool, array, object, any).
    pub param_type: String,
    /// Whether this parameter is required.
    pub required: bool,
    /// Default value if not required.
    pub default: Option<Value>,
    /// Description for help text.
    pub description: String,
    /// Alternative names/flags for this parameter (e.g., "-r", "-R" for "recursive").
    pub aliases: Vec<String>,
    /// Number of positional tokens this non-bool flag consumes per occurrence.
    ///
    /// Default 1 (standard `--flag value`). Set to 2 for `--flag NAME VALUE`
    /// patterns such as jq's `--arg` / `--argjson`. When `consumes > 1`, the
    /// kernel collects each occurrence as an inner array and accumulates
    /// repeated occurrences under the same `named` key — the tool sees a
    /// `Value::Json(Array(Array(...)))` listing every (N-tuple) occurrence.
    #[serde(default = "default_consumes")]
    pub consumes: usize,
    /// True for positional arguments (`cat foo.txt`), false for flags
    /// (`grep --ignore-case`). The validator matches positional params
    /// against `args.positional` by their order *among positionals only*,
    /// independent of where they sit in the clap struct. Default false so
    /// hand-built `ParamSchema::required(...)` constructors keep flag
    /// semantics; clap-reflected positionals set it via
    /// `arg.get_index().is_some()`.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub positional: bool,
}

impl ParamSchema {
    /// Create a required parameter.
    pub fn required(name: impl Into<String>, param_type: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            param_type: param_type.into(),
            required: true,
            default: None,
            description: description.into(),
            aliases: Vec::new(),
            consumes: 1,
            positional: false,
        }
    }

    /// Create an optional parameter with a default value.
    pub fn optional(name: impl Into<String>, param_type: impl Into<String>, default: Value, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            param_type: param_type.into(),
            required: false,
            default: Some(default),
            description: description.into(),
            aliases: Vec::new(),
            consumes: 1,
            positional: false,
        }
    }

    /// Mark this parameter as positional (matched by argv order rather than
    /// by name). Used by `params_from_clap` for clap args with an assigned
    /// index, and by hand-written schemas for positional parameters like
    /// jq's `filter`.
    pub fn positional(mut self) -> Self {
        self.positional = true;
        self
    }

    /// Add alternative names/flags for this parameter.
    ///
    /// Aliases are used for short flags like `-r`, `-R` that map to `recursive`.
    pub fn with_aliases(mut self, aliases: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.aliases = aliases.into_iter().map(Into::into).collect();
        self
    }

    /// Declare how many positional tokens this non-bool flag consumes per
    /// occurrence (`--flag v1 v2 ...`). Default is 1. Panics on 0 — a flag
    /// that consumes nothing is a bool flag, not a schema-typed param.
    pub fn consumes(mut self, n: usize) -> Self {
        assert!(n >= 1, "ParamSchema::consumes requires n >= 1 (use a bool param for flags that take no value)");
        self.consumes = n;
        self
    }

    /// Check if a flag name matches this parameter or any of its aliases.
    pub fn matches_flag(&self, flag: &str) -> bool {
        if self.name == flag {
            return true;
        }
        self.aliases.iter().any(|a| a == flag)
    }
}

/// An example showing how to use a tool.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Example {
    /// Short description of what the example demonstrates.
    pub description: String,
    /// The example command/code.
    pub code: String,
}

impl Example {
    /// Create a new example.
    pub fn new(description: impl Into<String>, code: impl Into<String>) -> Self {
        Self {
            description: description.into(),
            code: code.into(),
        }
    }
}

/// Schema describing a tool's interface.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ToolSchema {
    /// Tool name.
    pub name: String,
    /// Short description.
    pub description: String,
    /// Parameter definitions.
    pub params: Vec<ParamSchema>,
    /// Usage examples.
    pub examples: Vec<Example>,
    /// Map remaining positional args to named params by schema order.
    /// Only for MCP/external tools that expect named JSON params.
    /// Builtins handle their own positionals and should leave this false.
    pub map_positionals: bool,
}

impl ToolSchema {
    /// Create a new tool schema.
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            params: Vec::new(),
            examples: Vec::new(),
            map_positionals: false,
        }
    }

    /// Enable positional->named parameter mapping for MCP/external tools.
    pub fn with_positional_mapping(mut self) -> Self {
        self.map_positionals = true;
        self
    }

    /// Add a parameter to the schema.
    pub fn param(mut self, param: ParamSchema) -> Self {
        self.params.push(param);
        self
    }

    /// Add an example to the schema.
    pub fn example(mut self, description: impl Into<String>, code: impl Into<String>) -> Self {
        self.examples.push(Example::new(description, code));
        self
    }
}

/// Parsed arguments ready for tool execution.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct ToolArgs {
    /// Positional arguments in order.
    pub positional: Vec<Value>,
    /// Named arguments by key.
    pub named: BTreeMap<String, Value>,
    /// Boolean flags (e.g., -l, --force).
    pub flags: HashSet<String>,
}

impl ToolArgs {
    /// Create empty args.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get a positional argument by index.
    pub fn get_positional(&self, index: usize) -> Option<&Value> {
        self.positional.get(index)
    }

    /// Get a named argument by key.
    pub fn get_named(&self, key: &str) -> Option<&Value> {
        self.named.get(key)
    }

    /// Get a named argument or positional fallback.
    ///
    /// Useful for tools that accept both `cat file.txt` and `cat path=file.txt`.
    pub fn get(&self, name: &str, positional_index: usize) -> Option<&Value> {
        self.named.get(name).or_else(|| self.positional.get(positional_index))
    }

    /// Get a string value from args.
    pub fn get_string(&self, name: &str, positional_index: usize) -> Option<String> {
        self.get(name, positional_index).and_then(|v| match v {
            Value::String(s) => Some(s.clone()),
            Value::Int(i) => Some(i.to_string()),
            Value::Float(f) => Some(f.to_string()),
            Value::Bool(b) => Some(b.to_string()),
            _ => None,
        })
    }

    /// Get a boolean value from args.
    pub fn get_bool(&self, name: &str, positional_index: usize) -> Option<bool> {
        self.get(name, positional_index).and_then(|v| match v {
            Value::Bool(b) => Some(*b),
            Value::String(s) => match s.as_str() {
                "true" | "yes" | "1" => Some(true),
                "false" | "no" | "0" => Some(false),
                _ => None,
            },
            Value::Int(i) => Some(*i != 0),
            _ => None,
        })
    }

    /// Check if a flag is set (in flags set, or named bool).
    pub fn has_flag(&self, name: &str) -> bool {
        // Check the flags set first (from -x or --name syntax)
        if self.flags.contains(name) {
            return true;
        }
        // Fall back to checking named args (from name=true syntax)
        self.named.get(name).is_some_and(|v| match v {
            Value::Bool(b) => *b,
            Value::String(s) => !s.is_empty() && s != "false" && s != "0",
            _ => true,
        })
    }

    /// Move bool entries from `named` into the appropriate set so a downstream
    /// clap parser (with `#[arg(...)] field: bool`) accepts them.
    ///
    /// Tests routinely seed `args.named.insert(K, Value::Bool(true))` for the
    /// schema-pre-clap path; `to_argv()` would emit those as `--K=true`, which
    /// clap rejects for `bool` fields. Promote to:
    /// - `Bool(true)` → presence in `flags` (clap sees `--K`).
    /// - `Bool(false)` → dropped (clap treats absent flag and explicit false
    ///   the same; preserving it would only resurface as `--K=false` and break
    ///   the same parser).
    ///
    /// Idempotent. Non-bool named entries are left alone.
    pub fn flagify_bool_named(&mut self) {
        let bool_keys: Vec<String> = self
            .named
            .iter()
            .filter(|(_, v)| matches!(v, Value::Bool(_)))
            .map(|(k, _)| k.clone())
            .collect();
        for k in bool_keys {
            // Remove unconditionally so Bool(false) doesn't linger and break
            // a `--K=false` rejection in clap. Only Bool(true) re-enters as a
            // flag presence.
            if let Some(Value::Bool(true)) = self.named.remove(&k) {
                self.flags.insert(k);
            }
        }
    }

    /// Reconstruct a clap-friendly argv vector from already-parsed ToolArgs.
    ///
    /// kaish has already done shell parsing (variables expanded, globs expanded,
    /// `$(...)` substituted, schema-driven flag/value splitting). `to_argv`
    /// rebuilds a flat token stream suitable for `Parser::parse_from(std::iter::once("<tool>").chain(args.to_argv()))`.
    ///
    /// Layout: flags first (as `--<name>`), then named values (as
    /// `--<name>=<value>`), then positionals — separated from earlier sections
    /// by `--` so trailing-passthrough builtins still see them as positionals
    /// even if a value happens to begin with `-`.
    ///
    /// See docs/clap-migration.md for the full recipe.
    pub fn to_argv(&self) -> Vec<String> {
        let mut argv = Vec::with_capacity(
            self.flags.len() + self.named.len() * 2 + self.positional.len() + 1,
        );

        // Flags are unordered (HashSet); sort for deterministic argv so tests
        // and snapshots stay stable. Single-char keys emit short form (`-n`)
        // so clap's natural `#[arg(short = 'n', long = "no_newline")]` derive
        // accepts them without needing visible_alias gymnastics.
        let mut flags: Vec<&String> = self.flags.iter().collect();
        flags.sort();
        for flag in flags {
            argv.push(flag_token(flag));
        }

        // Named values: emit `-k=value` for single-char keys and `--key=value`
        // for multi-char keys. `=` form keeps parsing unambiguous when the
        // value begins with `-`. Multi-value (`consumes > 1`) params are
        // stored as Value::Json(Array(Array(...))) — one entry per occurrence.
        for (key, value) in &self.named {
            for rendered in render_named_value(value) {
                argv.push(format!("{}={}", flag_token(key), rendered));
            }
        }

        // `--` terminator so clap treats positionals as positionals even if
        // they begin with `-` (e.g. `echo -- -n` should print `-n`).
        if !self.positional.is_empty() {
            argv.push("--".to_string());
            for value in &self.positional {
                argv.push(value_to_argv_token(value));
            }
        }

        argv
    }
}

fn flag_token(name: &str) -> String {
    if name.chars().count() == 1 {
        format!("-{name}")
    } else {
        format!("--{name}")
    }
}

fn render_named_value(value: &Value) -> Vec<String> {
    match value {
        // `consumes > 1` lands as Json(Array(Array(...))) — one inner array per
        // occurrence. Flatten each inner array into space-joined tokens; clap
        // can split on `=` further if needed.
        Value::Json(serde_json::Value::Array(outer)) if outer.iter().all(|v| v.is_array()) => {
            outer
                .iter()
                .map(|inner| {
                    inner
                        .as_array()
                        .map(|a| a.iter().map(json_value_to_token).collect::<Vec<_>>().join(" "))
                        .unwrap_or_default()
                })
                .collect()
        }
        _ => vec![value_to_argv_token(value)],
    }
}

fn value_to_argv_token(value: &Value) -> String {
    match value {
        Value::Null => String::new(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
        Value::Json(j) => j.to_string(),
        Value::Blob(b) => format!("[blob: {} {}]", b.formatted_size(), b.content_type),
    }
}

fn json_value_to_token(value: &serde_json::Value) -> String {
    match value {
        serde_json::Value::Null => String::new(),
        serde_json::Value::Bool(b) => b.to_string(),
        serde_json::Value::Number(n) => n.to_string(),
        serde_json::Value::String(s) => s.clone(),
        other => other.to_string(),
    }
}

#[cfg(test)]
mod to_argv_tests {
    use super::*;

    #[test]
    fn empty_args_produce_empty_argv() {
        assert!(ToolArgs::new().to_argv().is_empty());
    }

    #[test]
    fn positionals_emitted_after_double_dash() {
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("world".into()));
        assert_eq!(args.to_argv(), vec!["--", "hello", "world"]);
    }

    #[test]
    fn single_char_flags_emit_short_form() {
        let mut args = ToolArgs::new();
        args.flags.insert("n".into());
        args.flags.insert("verbose".into());
        // Sorted: "n" then "verbose"
        assert_eq!(args.to_argv(), vec!["-n", "--verbose"]);
    }

    #[test]
    fn named_values_use_equals_form() {
        let mut args = ToolArgs::new();
        args.named.insert("count".into(), Value::Int(5));
        args.named.insert("name".into(), Value::String("foo".into()));
        // BTreeMap iterates in key order, so "count" before "name"
        assert_eq!(args.to_argv(), vec!["--count=5", "--name=foo"]);
    }

    #[test]
    fn single_char_named_emits_short_equals() {
        let mut args = ToolArgs::new();
        args.named.insert("n".into(), Value::Int(5));
        assert_eq!(args.to_argv(), vec!["-n=5"]);
    }

    #[test]
    fn positional_with_leading_dash_survives_double_dash() {
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-n".into()));
        // `echo -- -n` should round-trip as `-- -n`, not be reparsed as a flag.
        assert_eq!(args.to_argv(), vec!["--", "-n"]);
    }

    #[test]
    fn mixed_flags_named_positionals() {
        let mut args = ToolArgs::new();
        args.flags.insert("verbose".into());
        args.named.insert("limit".into(), Value::Int(10));
        args.positional.push(Value::String("file.txt".into()));
        assert_eq!(
            args.to_argv(),
            vec!["--verbose", "--limit=10", "--", "file.txt"]
        );
    }

    #[test]
    fn flagify_bool_named_promotes_true_to_flag() {
        let mut args = ToolArgs::new();
        args.named.insert("recursive".into(), Value::Bool(true));
        args.named.insert("limit".into(), Value::Int(5));

        args.flagify_bool_named();

        assert!(args.flags.contains("recursive"));
        assert!(!args.named.contains_key("recursive"));
        // Non-bool entries are untouched.
        assert_eq!(args.named.get("limit"), Some(&Value::Int(5)));
    }

    #[test]
    fn flagify_bool_named_drops_false() {
        let mut args = ToolArgs::new();
        args.named.insert("recursive".into(), Value::Bool(false));

        args.flagify_bool_named();

        assert!(!args.flags.contains("recursive"));
        assert!(!args.named.contains_key("recursive"));
    }

    #[test]
    fn flagify_bool_named_is_idempotent() {
        let mut args = ToolArgs::new();
        args.named.insert("recursive".into(), Value::Bool(true));
        args.flagify_bool_named();
        args.flagify_bool_named();
        assert!(args.flags.contains("recursive"));
    }

    /// Regression guard: argv emitted after flagify must round-trip through
    /// a clap parser without `--K=true` showing up.
    #[test]
    fn flagify_bool_named_round_trips_through_to_argv() {
        let mut args = ToolArgs::new();
        args.named.insert("R".into(), Value::Bool(true));
        args.flagify_bool_named();
        let argv = args.to_argv();
        assert!(argv.contains(&"-R".to_string()), "expected -R, got {:?}", argv);
        assert!(!argv.iter().any(|s| s.contains('=')), "no =value should appear, got {:?}", argv);
    }
}
