//! Tool schema and argument types.

use std::collections::{BTreeMap, HashSet};

use crate::value::Value;

fn default_consumes() -> usize {
    1
}

/// Schema for a tool parameter.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
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
    /// True when this flag may appear more than once and each occurrence
    /// should be kept (clap's `ArgAction::Append`, i.e. a `Vec<_>` value flag
    /// like sed's `-e`). When set, the kernel accumulates every occurrence
    /// under the same `named` key as a `Value::Json(Array(...))` instead of
    /// letting the last write win — the "no silent drop" contract for repeated
    /// flags. Orthogonal to `consumes`: `consumes` is values-per-occurrence,
    /// `repeatable` is occurrences-per-invocation.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub repeatable: bool,
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
            repeatable: false,
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
            repeatable: false,
            positional: false,
        }
    }

    /// Create a minimal parameter (not required, no default, empty
    /// description, `consumes` 1, flag — not positional). Chain the `with_*`
    /// setters to fill in fields. Use this when each field is computed
    /// independently (e.g. reflected from clap) rather than fitting the
    /// `required`/`optional` shortcuts. Keeps construction working across the
    /// `#[non_exhaustive]` boundary.
    pub fn new(name: impl Into<String>, param_type: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            param_type: param_type.into(),
            required: false,
            default: None,
            description: String::new(),
            aliases: Vec::new(),
            consumes: 1,
            repeatable: false,
            positional: false,
        }
    }

    /// Set the human-readable description.
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = description.into();
        self
    }

    /// Set whether the parameter is required.
    pub fn with_required(mut self, required: bool) -> Self {
        self.required = required;
        self
    }

    /// Set the default value (used when the parameter is omitted).
    pub fn with_default(mut self, default: Option<Value>) -> Self {
        self.default = default;
        self
    }

    /// Set the positional flag from a computed boolean (the parameterless
    /// [`positional`](Self::positional) sets it unconditionally to `true`).
    pub fn with_positional(mut self, positional: bool) -> Self {
        self.positional = positional;
        self
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

    /// Mark this flag as repeatable: each occurrence is accumulated rather than
    /// overwritten (see [`repeatable`](Self::repeatable)). Set from a computed
    /// boolean so clap reflection can pass `ArgAction::Append` directly.
    pub fn with_repeatable(mut self, repeatable: bool) -> Self {
        self.repeatable = repeatable;
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
#[non_exhaustive]
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
    /// Child schemas for subcommand-aware tools (`kj context list`, …).
    ///
    /// Empty for flat tools (`cat`, `grep`, `ls`) — they take the flat binding
    /// path. When non-empty, the kernel walks leading positionals to pick the
    /// active leaf and binds flags against *that leaf's* `params` (see
    /// `select_leaf` in the kernel).
    ///
    /// `skip_serializing_if` keeps the wire compact for the many flat tools
    /// (no `"subcommands":[]` noise); `default` is then required so a flat
    /// tool's payload (key absent) deserializes back to empty.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub subcommands: Vec<ToolSchema>,
    /// Command-level aliases (`ls` → `list`, `rm` → `remove`), matched when
    /// routing a positional to a child. Distinct from [`ParamSchema::aliases`],
    /// which name *flags*.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub aliases: Vec<String>,
    /// The tool renders its **own** output, including `--json` — the kernel
    /// must not re-format its `ExecResult` through `apply_output_format`.
    ///
    /// Default false: a tool returns typed [`crate::OutputData`] and the kernel
    /// renders the requested format uniformly. Set true for tools with bespoke
    /// JSON envelopes (e.g. an embedder's `kj`): they consume `--json`
    /// themselves and emit final bytes. See [`ToolSchema::with_owned_output`].
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub owns_output: bool,
    /// The tool wants its argv **in source order, with types preserved** — the
    /// binder must NOT split flags into the unordered `flags` set. When true,
    /// every argument is bound to `positional` in the order written (operators
    /// like `-f`/`=`/`!` as strings, operands keeping their `Value` type), and
    /// `named`/`flags` stay empty.
    ///
    /// Default false: normal tools get the clap-style order-independent split
    /// (`-la` == `-al`). Set true for the rare *position-sensitive* command
    /// whose operands may themselves look like flags — POSIX `test`, where
    /// `test $x = -n` and `test 0 -gt -5` must see `-n`/`-5` as literal
    /// operands. See [`ToolSchema::with_raw_argv`].
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub raw_argv: bool,
    /// The tool consumes glob patterns **as data** — the argv binder must pass
    /// a bare glob pattern through as literal text instead of expanding it to
    /// matching paths.
    ///
    /// Default false: shell semantics — `cat *.rs` sees matching files and
    /// zero matches is a bind-time error. Set true for a tool whose input *is*
    /// the pattern (`glob`), so the natural unquoted spelling
    /// (`glob **/*.rs`) hands the pattern text to the tool instead of walking
    /// the tree at bind time and binding the first match as the "pattern".
    /// See [`ToolSchema::with_glob_passthrough`].
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub glob_passthrough: bool,
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
            subcommands: Vec::new(),
            aliases: Vec::new(),
            owns_output: false,
            raw_argv: false,
            glob_passthrough: false,
        }
    }

    /// Declare that this tool wants its argv in source order with types
    /// preserved (no flag/positional split). See [`ToolSchema::raw_argv`].
    pub fn with_raw_argv(mut self) -> Self {
        self.raw_argv = true;
        self
    }

    /// Declare that this tool consumes glob patterns as data: the argv binder
    /// passes bare patterns through as literal text instead of expanding them.
    /// See [`ToolSchema::glob_passthrough`].
    pub fn with_glob_passthrough(mut self) -> Self {
        self.glob_passthrough = true;
        self
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

    /// Add a child schema, making this a subcommand-aware tool.
    pub fn subcommand(mut self, child: ToolSchema) -> Self {
        self.subcommands.push(child);
        self
    }

    /// Set command-level aliases (e.g. `ls` for a `list` subcommand). These
    /// name the *command*, not its flags; flag aliases live on each
    /// [`ParamSchema`].
    pub fn with_command_aliases(mut self, aliases: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.aliases = aliases.into_iter().map(Into::into).collect();
        self
    }

    /// True if `word` names this command — its `name` or any of its
    /// command-level `aliases`. Used when routing a positional to a child.
    pub fn matches_command(&self, word: &str) -> bool {
        self.name == word || self.aliases.iter().any(|a| a == word)
    }

    /// Declare that this tool renders its own output (including `--json`), so
    /// the kernel won't re-format its result.
    ///
    /// Applies to the whole tree: every subcommand is marked too, and a `json`
    /// param is advertised on each node that doesn't already declare one.
    /// Reflection skips `json` as the kernel-global output flag, so this
    /// re-advertises it for tools that genuinely own it — closing the loop so
    /// `help <tool> <sub>` lists `--json` where the tool actually handles it.
    pub fn with_owned_output(mut self) -> Self {
        self.mark_owned_output();
        self
    }

    fn mark_owned_output(&mut self) {
        self.owns_output = true;
        if !self.params.iter().any(|p| p.name == "json") {
            self.params.push(
                ParamSchema::new("json", "bool").with_description("Render output as JSON"),
            );
        }
        for child in &mut self.subcommands {
            child.mark_owned_output();
        }
    }
}

/// Parsed arguments ready for tool execution.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
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
    /// A `Value::Bool` parked under a key the `schema` declares as a *value-taking*
    /// flag is the flag's literal value, not a bare bool flag — `spawn --command
    /// true` binds `command = Bool(true)`. Those keys are left in `named` so
    /// `to_argv()` renders `--command=true` and clap's `Option<String>` field
    /// accepts it; collapsing them to a bare `--command` drops the value and
    /// makes clap error "a value is required". (See docs/issues.md.)
    ///
    /// Idempotent. Non-bool named entries are left alone.
    pub fn flagify_bool_named(&mut self, schema: &ToolSchema) {
        // Keys (param names + aliases) the schema declares as non-bool, non-positional
        // flags — i.e. flags that take a value.
        let value_keys: HashSet<&str> = schema
            .params
            .iter()
            .filter(|p| !p.positional && !is_bool_param_type(&p.param_type))
            .flat_map(|p| {
                std::iter::once(p.name.as_str())
                    .chain(p.aliases.iter().map(|a| a.trim_start_matches('-')))
            })
            .collect();

        let bool_keys: Vec<String> = self
            .named
            .iter()
            .filter(|(k, v)| matches!(v, Value::Bool(_)) && !value_keys.contains(k.as_str()))
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
    /// rebuilds a flat token stream suitable for `Parser::parse_from(std::iter::once("<tool>").chain(args.to_argv()?))`.
    ///
    /// Layout: flags first (as `--<name>`), then named values (as
    /// `--<name>=<value>`), then positionals — separated from earlier sections
    /// by `--` so trailing-passthrough builtins still see them as positionals
    /// even if a value happens to begin with `-`.
    ///
    /// # Errors
    ///
    /// Returns [`ToolArgvError`] when a **named/flag** value is
    /// [`Value::Bytes`] — binary can't cross the argv/text stringification
    /// boundary (GH #164, closing the root cause behind GH #120's stringified
    /// `[binary: N bytes]` placeholder). A **positional** `Value::Bytes` does
    /// NOT error here; see [`value_to_argv_token`]'s doc comment for why.
    ///
    /// See the clap builtin pattern in CLAUDE.md (Contributor conventions).
    ///
    /// Equivalent to [`to_argv_excluding`](Self::to_argv_excluding)`(&[])` —
    /// same rendering path, nothing excluded.
    pub fn to_argv(&self) -> Result<Vec<String>, ToolArgvError> {
        self.to_argv_excluding(&[])
    }

    /// Like [`to_argv`](Self::to_argv), but skips the given **named** keys
    /// entirely — neither the key's flag token nor its value appears in the
    /// rendered argv, and (crucially) a `Value::Bytes` under an excluded key
    /// is never passed to [`render_named_value`], so it can never trip
    /// [`ToolArgvError::BinaryNamedValue`].
    ///
    /// Use this when a builtin deliberately reads one of its own named
    /// parameters raw off `ToolArgs` (e.g. `args.named.get("content")`)
    /// instead of the clap-parsed field, specifically to preserve a
    /// typed/binary value that must not cross the argv/text stringification
    /// boundary — while still wanting the *rest* of its arguments bound
    /// through the normal clap path. `write`'s `content` param is the
    /// motivating case (GH #218, a follow-up from the GH #164 / #215
    /// review): before this helper, the builtin cloned the whole `ToolArgs`
    /// and called `named.remove("content")` by hand, which silently stops
    /// covering a *second* Bytes-capable named param the moment one is added.
    /// Naming the excluded keys here instead makes the exemption a
    /// greppable, drift-resistant idiom.
    ///
    /// Only **named** keys are excludable — not flags or positionals, by
    /// design. A bool flag carries no value to protect, so there is nothing
    /// to exempt. A positional's clap-reflected field is already a
    /// validation-only sink nobody reads (see CLAUDE.md's clap-builtin
    /// convention), so a positional `Value::Bytes` never needed an
    /// exemption in the first place — [`value_to_argv_token`] renders it as
    /// an inert placeholder rather than erroring. If a future case needs to
    /// exclude a flag or positional too, that is new design, not an
    /// extension of this helper.
    pub fn to_argv_excluding(&self, exclude: &[&str]) -> Result<Vec<String>, ToolArgvError> {
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
        // An excluded key is skipped before its value is ever inspected, so a
        // Value::Bytes there can't reach render_named_value's Bytes guard.
        for (key, value) in &self.named {
            if exclude.contains(&key.as_str()) {
                continue;
            }
            for rendered in render_named_value(key, value)? {
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

        Ok(argv)
    }
}

/// Error raised by [`ToolArgs::to_argv`] when a **named or flag** argument
/// cannot cross the argv/text stringification boundary.
///
/// The only offending [`Value`] variant is [`Value::Bytes`] — every other
/// variant has a lossless text form. Binary crossing this boundary used to
/// silently render as a `[binary: N bytes]` placeholder text token, which a
/// downstream clap-parsed field (`parsed.separator`, `parsed.algo`, …) would
/// then see as if it were the user's real value — GH #120's root cause,
/// deferred as "Phase 2" at the time and closed here (GH #164).
///
/// Deliberately does **not** cover positional `Value::Bytes` — see
/// [`value_to_argv_token`]'s doc comment for why a positional binary value
/// stays safe to render as a placeholder rather than error.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[non_exhaustive]
pub enum ToolArgvError {
    /// A named/flag argument held [`Value::Bytes`].
    #[error(
        "argument `{key}` holds {byte_len} binary bytes, which cannot cross the argv/text \
         boundary — read it from the raw ToolArgs value (e.g. `args.get(\"{key}\", ..)`) \
         instead of the clap-parsed field"
    )]
    BinaryNamedValue {
        /// The named argument's key (the schema name, e.g. `"separator"` —
        /// not the `-`/`--`-prefixed flag_token form).
        key: String,
        /// The number of binary bytes it held. Never the bytes themselves —
        /// this error message must stay safe to log.
        byte_len: usize,
    },
}

fn flag_token(name: &str) -> String {
    if name.chars().count() == 1 {
        format!("-{name}")
    } else {
        format!("--{name}")
    }
}

/// Whether a `ParamSchema::param_type` names a boolean flag.
fn is_bool_param_type(param_type: &str) -> bool {
    param_type.eq_ignore_ascii_case("bool") || param_type.eq_ignore_ascii_case("boolean")
}

/// Render one named argument's value into its `to_argv()` token(s).
///
/// `key` is only used to attribute a [`ToolArgvError`] to the argument that
/// held it — it does not affect rendering of any other variant.
fn render_named_value(key: &str, value: &Value) -> Result<Vec<String>, ToolArgvError> {
    match value {
        // `consumes > 1` lands as Json(Array(Array(...))) — one inner array per
        // occurrence. Flatten each inner array into space-joined tokens; clap
        // can split on `=` further if needed.
        Value::Json(serde_json::Value::Array(outer)) if outer.iter().all(|v| v.is_array()) => {
            Ok(outer
                .iter()
                .map(|inner| {
                    inner
                        .as_array()
                        .map(|a| a.iter().map(json_value_to_token).collect::<Vec<_>>().join(" "))
                        .unwrap_or_default()
                })
                .collect())
        }
        // A named/flag value is commonly read straight off the clap-parsed
        // field (`parsed.separator`, `parsed.algo`, …) rather than the raw
        // `ToolArgs`, so silently stringifying binary here — as the old
        // `[binary: N bytes]` placeholder did — hands a builtin's clap struct
        // a value that looks textual but isn't the user's real data (GH #120's
        // root cause). Loud instead: see `ToolArgvError`.
        Value::Bytes(data) => Err(ToolArgvError::BinaryNamedValue {
            key: key.to_string(),
            byte_len: data.len(),
        }),
        _ => Ok(vec![value_to_argv_token(value)]),
    }
}

/// Render one **positional** argument's value into its `to_argv()` token.
///
/// `Value::Bytes` renders as a visible placeholder rather than erroring —
/// unlike the named-value path in [`render_named_value`]. This is safe only
/// because a clap-reflected positional field is a validation-only sink (see
/// CLAUDE.md's clap-builtin convention): no builtin reads a positional's
/// *value* off the parsed clap struct, every one of them reads the typed
/// `Value` straight off `args.positional` instead (e.g. `push`'s `rest:
/// Vec<String>` sink, or `write`'s content positional, which accepts real
/// `Value::Bytes` content byte-for-byte via `args.positional`, never via
/// `parsed`). A placeholder token here only has to satisfy clap's parse (argv
/// shape / arity), never carry real data anywhere — so it can never leak
/// unlike the named case this function's sibling guards against.
fn value_to_argv_token(value: &Value) -> String {
    match value {
        Value::Null => String::new(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
        Value::Json(j) => j.to_string(),
        Value::Bytes(data) => format!("[binary: {} bytes]", data.len()),
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
mod schema_serde_tests {
    use super::*;

    /// A flat tool (no subcommands/aliases) must serialize byte-identically to
    /// the pre-subcommand wire format: the two new fields are skipped entirely.
    #[test]
    fn flat_schema_omits_new_fields_on_wire() {
        let schema = ToolSchema::new("cat", "concatenate")
            .param(ParamSchema::required("path", "string", "file to read").positional());
        let json = serde_json::to_value(&schema).expect("serialize");
        let obj = json.as_object().expect("object");
        assert!(!obj.contains_key("subcommands"), "flat tool leaks subcommands: {json}");
        assert!(!obj.contains_key("aliases"), "flat tool leaks command aliases: {json}");
    }

    /// Round-trip the skip: a flat tool serializes *without* the keys, so the
    /// deserializer must `default` them back to empty. (This is what lets us
    /// skip-serialize empties without breaking our own flat tools' payloads.)
    #[test]
    fn flat_wire_form_deserializes_to_empty() {
        let flat = serde_json::json!({
            "name": "cat",
            "description": "concatenate",
            "params": [],
            "examples": [],
            "map_positionals": false
        });
        let schema: ToolSchema = serde_json::from_value(flat).expect("deserialize flat form");
        assert!(schema.subcommands.is_empty());
        assert!(schema.aliases.is_empty());
    }

    /// `with_owned_output` marks the whole tree and advertises `json` on each
    /// node that didn't already declare it.
    #[test]
    fn with_owned_output_marks_tree_and_advertises_json() {
        let schema = ToolSchema::new("kj", "kaijutsu")
            .subcommand(
                ToolSchema::new("context", "ctx")
                    .subcommand(ToolSchema::new("list", "list contexts")),
            )
            .with_owned_output();

        assert!(schema.owns_output, "root marked");
        assert!(schema.params.iter().any(|p| p.name == "json"), "root advertises json");
        let context = &schema.subcommands[0];
        assert!(context.owns_output, "child marked");
        let list = &context.subcommands[0];
        assert!(list.owns_output, "grandchild marked");
        assert!(list.params.iter().any(|p| p.name == "json"), "leaf advertises json");
    }

    /// `with_owned_output` doesn't duplicate an already-declared `json` param.
    #[test]
    fn with_owned_output_does_not_double_add_json() {
        let schema = ToolSchema::new("kj", "kaijutsu")
            .param(ParamSchema::new("json", "bool"))
            .with_owned_output();
        let json_count = schema.params.iter().filter(|p| p.name == "json").count();
        assert_eq!(json_count, 1, "json should appear exactly once");
    }

    /// `owns_output` round-trips and is omitted from the wire when false.
    #[test]
    fn owns_output_serde() {
        let flat = ToolSchema::new("ls", "list");
        let json = serde_json::to_value(&flat).expect("serialize");
        let obj = json.as_object().expect("object");
        assert!(!obj.contains_key("owns_output"), "false omitted: {json}");

        let owned = ToolSchema::new("kj", "kaijutsu").with_owned_output();
        let wire = serde_json::to_string(&owned).expect("serialize");
        let back: ToolSchema = serde_json::from_str(&wire).expect("deserialize");
        assert!(back.owns_output);
    }

    /// A subcommand tree round-trips through serde with names and aliases intact.
    #[test]
    fn subcommand_tree_round_trips() {
        let schema = ToolSchema::new("kj", "kaijutsu")
            .subcommand(
                ToolSchema::new("context", "context ops")
                    .with_command_aliases(["ctx"])
                    .subcommand(ToolSchema::new("list", "list contexts").with_command_aliases(["ls"])),
            );
        let json = serde_json::to_string(&schema).expect("serialize");
        let back: ToolSchema = serde_json::from_str(&json).expect("deserialize");
        assert_eq!(back.subcommands.len(), 1);
        let context = &back.subcommands[0];
        assert!(context.matches_command("context"));
        assert!(context.matches_command("ctx"));
        assert_eq!(context.subcommands.len(), 1);
        assert!(context.subcommands[0].matches_command("ls"));
    }
}

#[cfg(test)]
mod to_argv_tests {
    use super::*;

    #[test]
    fn empty_args_produce_empty_argv() {
        assert!(ToolArgs::new().to_argv().unwrap().is_empty());
    }

    #[test]
    fn positionals_emitted_after_double_dash() {
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("world".into()));
        assert_eq!(args.to_argv().unwrap(), vec!["--", "hello", "world"]);
    }

    #[test]
    fn single_char_flags_emit_short_form() {
        let mut args = ToolArgs::new();
        args.flags.insert("n".into());
        args.flags.insert("verbose".into());
        // Sorted: "n" then "verbose"
        assert_eq!(args.to_argv().unwrap(), vec!["-n", "--verbose"]);
    }

    #[test]
    fn named_values_use_equals_form() {
        let mut args = ToolArgs::new();
        args.named.insert("count".into(), Value::Int(5));
        args.named.insert("name".into(), Value::String("foo".into()));
        // BTreeMap iterates in key order, so "count" before "name"
        assert_eq!(args.to_argv().unwrap(), vec!["--count=5", "--name=foo"]);
    }

    #[test]
    fn single_char_named_emits_short_equals() {
        let mut args = ToolArgs::new();
        args.named.insert("n".into(), Value::Int(5));
        assert_eq!(args.to_argv().unwrap(), vec!["-n=5"]);
    }

    #[test]
    fn positional_with_leading_dash_survives_double_dash() {
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-n".into()));
        // `echo -- -n` should round-trip as `-- -n`, not be reparsed as a flag.
        assert_eq!(args.to_argv().unwrap(), vec!["--", "-n"]);
    }

    #[test]
    fn mixed_flags_named_positionals() {
        let mut args = ToolArgs::new();
        args.flags.insert("verbose".into());
        args.named.insert("limit".into(), Value::Int(10));
        args.positional.push(Value::String("file.txt".into()));
        assert_eq!(
            args.to_argv().unwrap(),
            vec!["--verbose", "--limit=10", "--", "file.txt"]
        );
    }

    /// GH #164: a named/flag `Value::Bytes` must error loudly instead of
    /// silently stringifying to the `[binary: N bytes]` placeholder — that
    /// placeholder is exactly what a downstream clap-parsed field
    /// (`parsed.separator`, `parsed.algo`, …) would otherwise see as if it
    /// were the user's real value (GH #120's root cause).
    #[test]
    fn named_bytes_value_errors_loudly() {
        let mut args = ToolArgs::new();
        args.named.insert("separator".into(), Value::Bytes(vec![0xff, 0x00, 0xfe]));

        let err = args.to_argv().expect_err("named Bytes must error");
        // The message must name the key and the byte count.
        let message = err.to_string();
        assert!(message.contains("separator"));
        assert!(message.contains('3'));
        let ToolArgvError::BinaryNamedValue { key, byte_len } = err;
        assert_eq!(key, "separator");
        assert_eq!(byte_len, 3);
    }

    /// A single-char named key (e.g. `-a`) gets the same loud treatment.
    #[test]
    fn single_char_named_bytes_value_errors_loudly() {
        let mut args = ToolArgs::new();
        args.named.insert("a".into(), Value::Bytes(vec![1, 2]));

        let err = args.to_argv().expect_err("named Bytes must error");
        let ToolArgvError::BinaryNamedValue { key, byte_len } = err;
        assert_eq!(key, "a");
        assert_eq!(byte_len, 2);
    }

    /// Positional `Value::Bytes`, by contrast, does NOT error — see
    /// `value_to_argv_token`'s doc comment. The clap-reflected positional
    /// field is a validation-only sink; no builtin ever reads its *value* off
    /// the parsed struct (they read the typed `Value` straight off
    /// `args.positional`), so a placeholder token here is inert, not
    /// corruption. This is the load-bearing decision behind builtins like
    /// `push`/`write` accepting real binary content through positionals.
    #[test]
    fn positional_bytes_value_renders_placeholder_not_error() {
        let mut args = ToolArgs::new();
        args.positional.push(Value::Bytes(vec![0xff, 0x00, 0xfe]));

        let argv = args.to_argv().expect("positional Bytes must not error");
        assert_eq!(argv, vec!["--", "[binary: 3 bytes]"]);
    }

    /// Mixed case: a named Bytes error takes priority even when a positional
    /// Bytes value is also present (the loud path must not be starved by
    /// iteration order silently succeeding on the positional half first).
    #[test]
    fn named_bytes_errors_even_with_positional_bytes_present() {
        let mut args = ToolArgs::new();
        args.named.insert("check".into(), Value::Bytes(vec![9, 9]));
        args.positional.push(Value::Bytes(vec![1, 2, 3]));

        let err = args.to_argv().expect_err("named Bytes must still error");
        let ToolArgvError::BinaryNamedValue { key, .. } = err;
        assert_eq!(key, "check");
    }

    // ── GH #218: ToolArgs::to_argv_excluding ────────────────────────────
    //
    // write.rs reads its own `content` named param raw off `ToolArgs` to
    // preserve `Value::Bytes`, then needs the *rest* of its args through the
    // normal clap/to_argv path — these tests pin the helper that replaces the
    // ad hoc "clone ToolArgs, remove the key, call to_argv()" dance.

    /// A named `Value::Bytes` under an excluded key must not error and must
    /// not appear anywhere in the rendered argv — this is the whole point:
    /// `write`'s `content` carries real binary and must never reach argv.
    #[test]
    fn to_argv_excluding_skips_excluded_named_bytes_without_error() {
        let mut args = ToolArgs::new();
        args.named.insert("content".into(), Value::Bytes(vec![0xff, 0x00, 0xfe]));
        args.named.insert("path".into(), Value::String("dest.bin".into()));

        let argv = args
            .to_argv_excluding(&["content"])
            .expect("excluded named Bytes must not error");
        assert_eq!(argv, vec!["--path=dest.bin"]);
        assert!(
            argv.iter().all(|tok| !tok.contains("content")),
            "excluded key must not appear in argv at all: {argv:?}"
        );
    }

    /// A named `Value::Bytes` under a key that is NOT excluded still errors
    /// loudly, same as plain `to_argv()` — excluding one key must not blanket
    /// the whole named map.
    #[test]
    fn to_argv_excluding_still_errors_on_non_excluded_named_bytes() {
        let mut args = ToolArgs::new();
        args.named.insert("content".into(), Value::Bytes(vec![1, 2, 3]));
        args.named.insert("separator".into(), Value::Bytes(vec![9, 9]));

        let err = args
            .to_argv_excluding(&["content"])
            .expect_err("non-excluded named Bytes must still error");
        let ToolArgvError::BinaryNamedValue { key, .. } = err;
        assert_eq!(key, "separator");
    }

    /// Excluding a key that isn't a `Value::Bytes` at all (the common case —
    /// most invocations of `write` carry plain string content) still drops it
    /// from argv. The exclusion is unconditional on the key, not conditional
    /// on the value being binary.
    #[test]
    fn to_argv_excluding_drops_excluded_key_regardless_of_value_type() {
        let mut args = ToolArgs::new();
        args.named.insert("content".into(), Value::String("hello".into()));
        args.named.insert("path".into(), Value::String("dest.txt".into()));

        let argv = args.to_argv_excluding(&["content"]).expect("no error expected");
        assert_eq!(argv, vec!["--path=dest.txt"]);
    }

    /// An empty exclude list must behave *exactly* like `to_argv()` — same
    /// tokens, same order — across flags, named values, and positionals, on
    /// args that don't touch the Bytes edge case at all. `to_argv()` itself
    /// delegates to this with an empty slice, so this is also the guard that
    /// the delegation didn't change plain `to_argv()` behavior.
    #[test]
    fn to_argv_excluding_empty_list_matches_to_argv() {
        let mut args = ToolArgs::new();
        args.flags.insert("verbose".into());
        args.flags.insert("n".into());
        args.named.insert("limit".into(), Value::Int(10));
        args.named.insert("name".into(), Value::String("foo".into()));
        args.positional.push(Value::String("file.txt".into()));
        args.positional.push(Value::String("-weird".into()));

        assert_eq!(
            args.to_argv_excluding(&[]).unwrap(),
            args.to_argv().unwrap(),
            "empty exclude list must be indistinguishable from to_argv()"
        );
    }

    #[test]
    fn flagify_bool_named_promotes_true_to_flag() {
        let mut args = ToolArgs::new();
        args.named.insert("recursive".into(), Value::Bool(true));
        args.named.insert("limit".into(), Value::Int(5));

        args.flagify_bool_named(&ToolSchema::new("t", ""));

        assert!(args.flags.contains("recursive"));
        assert!(!args.named.contains_key("recursive"));
        // Non-bool entries are untouched.
        assert_eq!(args.named.get("limit"), Some(&Value::Int(5)));
    }

    #[test]
    fn flagify_bool_named_drops_false() {
        let mut args = ToolArgs::new();
        args.named.insert("recursive".into(), Value::Bool(false));

        args.flagify_bool_named(&ToolSchema::new("t", ""));

        assert!(!args.flags.contains("recursive"));
        assert!(!args.named.contains_key("recursive"));
    }

    #[test]
    fn flagify_bool_named_is_idempotent() {
        let mut args = ToolArgs::new();
        args.named.insert("recursive".into(), Value::Bool(true));
        args.flagify_bool_named(&ToolSchema::new("t", ""));
        args.flagify_bool_named(&ToolSchema::new("t", ""));
        assert!(args.flags.contains("recursive"));
    }

    /// Regression guard: argv emitted after flagify must round-trip through
    /// a clap parser without `--K=true` showing up.
    #[test]
    fn flagify_bool_named_round_trips_through_to_argv() {
        let mut args = ToolArgs::new();
        args.named.insert("R".into(), Value::Bool(true));
        args.flagify_bool_named(&ToolSchema::new("t", ""));
        let argv = args.to_argv().unwrap();
        assert!(argv.contains(&"-R".to_string()), "expected -R, got {:?}", argv);
        assert!(!argv.iter().any(|s| s.contains('=')), "no =value should appear, got {:?}", argv);
    }

    /// A `Bool(true)` parked under a schema-declared value-taking flag is the
    /// flag's literal value (`spawn --command true`), not a bare bool flag — it
    /// stays in `named` and renders as `--K=true`, not a value-less `--K`.
    #[test]
    fn flagify_bool_named_keeps_value_flag_value() {
        let mut schema = ToolSchema::new("spawn", "");
        schema.params.push(ParamSchema::new("command", "string"));

        let mut args = ToolArgs::new();
        args.named.insert("command".into(), Value::Bool(true));
        args.flagify_bool_named(&schema);

        assert!(!args.flags.contains("command"), "value flag must not collapse to a bare flag");
        assert_eq!(args.named.get("command"), Some(&Value::Bool(true)));
        let argv = args.to_argv().unwrap();
        assert!(
            argv.iter().any(|s| s == "--command=true"),
            "expected --command=true, got {:?}",
            argv
        );
    }

    /// One schema carrying both a bool flag and a value-taking flag: the bool
    /// flag still flagifies, the value flag keeps its value. Proves
    /// `is_bool_param_type` actually distinguishes the two (an empty-schema test
    /// can't — it flagifies everything regardless).
    #[test]
    fn flagify_bool_named_distinguishes_bool_from_value_param() {
        let mut schema = ToolSchema::new("t", "");
        schema.params.push(ParamSchema::new("verbose", "bool"));
        schema.params.push(ParamSchema::new("command", "string"));

        let mut args = ToolArgs::new();
        args.named.insert("verbose".into(), Value::Bool(true));
        args.named.insert("command".into(), Value::Bool(true));
        args.flagify_bool_named(&schema);

        // Bool flag → promoted to a bare flag.
        assert!(args.flags.contains("verbose"));
        assert!(!args.named.contains_key("verbose"));
        // Value flag → value retained.
        assert!(!args.flags.contains("command"));
        assert_eq!(args.named.get("command"), Some(&Value::Bool(true)));
    }
}
