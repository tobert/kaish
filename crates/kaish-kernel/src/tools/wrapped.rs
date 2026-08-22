//! Wrapped commands: external programs registered as kaish tools.
//!
//! The embedder declares the program, the verbs it allows, and the flags each
//! verb accepts. The kernel validates a call against that declaration and
//! renders the argv itself, so a value is never parsed as a flag by the child
//! unless the declaration put it in flag position.
//!
//! ```no_run
//! use kaish_kernel::tools::wrapped::{Flag, Positional, Verb, WrappedCommand};
//!
//! # fn main() -> anyhow::Result<()> {
//! let git = WrappedCommand::new("git")
//!     .executable("/usr/bin/git")
//!     .about("Version control, read-mostly.")
//!     .lead(["--no-pager"])
//!     .env("GIT_PAGER", "cat")
//!     .verb(Verb::new("log")
//!         .flag(Flag::value("max-count").alias("-n").int())
//!         .flag(Flag::switch("oneline"))
//!         .positional(Positional::many("revision")))
//!     .build()?;
//! # let _ = git;
//! # Ok(())
//! # }
//! ```
//!
//! See `docs/wrapped_command.md` for the design.
//!
//! # What this module does not do
//!
//! [`WrappedTool`] deliberately does **not** implement
//! [`Tool`](crate::tools::Tool). The declaration, the schema, the parse, the
//! render, and the constraint checks are here; spawning the child, streaming
//! its stdin, and turning its exit code into an `ExecResult` are the execute
//! half and land separately.

mod constraint;
mod declaration;
mod error;
mod parse;
mod render;

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use kaish_types::{ParamSchema, ToolArgs, ToolSchema, Value};

use kaish_tool_api::{IssueCode, Severity, ValidationIssue};

pub use declaration::{find_executable, Flag, Positional, Stdin, Style, Tail, Verb, WrappedCommand};
pub use error::WrappedError;

use constraint::resolve_under;

use parse::Word;

/// A `path_under` positional the call still has to satisfy.
///
/// An absolute value is decided when the call is planned; a relative one
/// needs the kernel's real cwd, which only the execute path holds.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct PathCheck {
    /// The declared positional's name.
    pub positional: String,
    /// The value as the agent wrote it.
    pub value: String,
    /// The declared root the resolved path must be inside.
    pub root: PathBuf,
    /// Where the value sits in [`RenderedCall::argv`], so the resolved path
    /// can replace it.
    pub argv_index: usize,
    /// True when planning already resolved the value and rewrote `argv`.
    pub resolved: bool,
}

/// A call the declaration accepts, rendered into the child's argv.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct RenderedCall {
    /// The selected verb's declared name; `None` for the root verb.
    pub verb: Option<String>,
    /// The child's argv, without the executable.
    pub argv: Vec<String>,
    /// What the child's standard input is connected to.
    pub stdin: Stdin,
    /// The verb declared its stdout to be JSON.
    pub json_output: bool,
    /// `path_under` positionals still to satisfy. Every one whose `resolved`
    /// is false needs the real cwd before the child runs.
    pub path_checks: Vec<PathCheck>,
}

/// A checked declaration with its executable pinned.
#[derive(Debug, Clone)]
pub struct WrappedTool {
    declaration: WrappedCommand,
    executable: PathBuf,
}

impl WrappedTool {
    pub(crate) fn from_parts(declaration: WrappedCommand, executable: PathBuf) -> Self {
        Self {
            declaration,
            executable,
        }
    }

    /// The tool name agents write.
    pub fn name(&self) -> &str {
        &self.declaration.name
    }

    /// The pinned executable, verified at `build()`.
    pub fn executable(&self) -> &Path {
        &self.executable
    }

    /// The environment pins for the child. They win over exported scope
    /// variables of the same name.
    pub fn env(&self) -> &BTreeMap<String, String> {
        &self.declaration.env
    }

    /// The checked declaration.
    pub fn declaration(&self) -> &WrappedCommand {
        &self.declaration
    }

    /// The schema the kernel publishes to agents.
    ///
    /// `raw_argv`, so the binder hands the wrapper every word in source order
    /// with `--` preserved — the wrapper's own grammar decides what each word
    /// is. Named verbs become subcommands; the root verb's flags and
    /// positionals sit on the root schema.
    pub fn schema(&self) -> ToolSchema {
        let mut schema = ToolSchema::new(&self.declaration.name, self.root_description());
        if let Some(root) = &self.declaration.root {
            for param in verb_params(root) {
                schema = schema.param(param);
            }
            for (label, command) in &root.examples {
                schema = schema.example(label, command);
            }
        }
        for (label, command) in &self.declaration.examples {
            schema = schema.example(label, command);
        }
        for verb in &self.declaration.verbs {
            schema = schema.subcommand(verb_schema(verb));
        }
        schema = schema.with_raw_argv();
        // The kernel reads `typed_substitution` from the root schema, so
        // `$(cmd …)` binds typed only when every verb the tool can run says
        // its stdout is JSON. A tool with one JSON verb among text ones keeps
        // the flag on that verb's schema for `help`, and a caller who wants
        // the data asks with `--json`.
        if self.every_verb_is_json() {
            schema = schema.with_typed_substitution();
        }
        schema
    }

    fn root_description(&self) -> String {
        match &self.declaration.root {
            Some(root) if root.tail == Tail::Forward => {
                append_clause(&self.declaration.about, "forwards undeclared flags")
            }
            _ => self.declaration.about.clone(),
        }
    }

    fn every_verb_is_json(&self) -> bool {
        let mut verbs = self
            .declaration
            .root
            .iter()
            .chain(self.declaration.verbs.iter())
            .peekable();
        verbs.peek().is_some() && verbs.all(|verb| verb.json_output)
    }

    /// Plan a call: parse it, check every constraint, and render the argv.
    ///
    /// Fails with the first refusal. Nothing spawns; every refusal exits 2.
    pub fn plan_call(&self, args: &ToolArgs) -> Result<RenderedCall, WrappedError> {
        let words = self.execution_words(args)?;
        let call = parse::parse(&self.declaration, &words).map_err(|failure| failure.error)?;
        let Some(verb) = call.verb(&self.declaration) else {
            // `select_verb` only declines when a word is opaque, and execution
            // has no opaque words.
            return Err(WrappedError::MissingVerb {
                command: self.declaration.name.clone(),
                allowed: parse::allowed_verbs(&self.declaration),
            });
        };
        if let Some(error) = constraint::check(&self.declaration, verb, &call).into_iter().next() {
            return Err(error);
        }

        let rendered = render::render(&self.declaration, verb, &call);
        let mut argv = rendered.argv;
        let mut path_checks = constraint::path_checks(verb, &call, &rendered.item_argv_index);

        // An absolute value is decided now: validation rejects a path outside
        // its root before anything spawns. A relative one waits for the real
        // cwd, which only the execute path holds.
        for check in &mut path_checks {
            if !Path::new(&check.value).is_absolute() {
                continue;
            }
            let resolved = resolve_under(&check.value, Path::new("/"), &check.root)
                .map_err(|e| e.attributed_to(&self.declaration.name, &check.positional))?;
            if let Some(word) = argv.get_mut(check.argv_index) {
                *word = resolved.to_string_lossy().into_owned();
            }
            check.resolved = true;
        }

        Ok(RenderedCall {
            verb: verb.name.clone(),
            argv,
            stdin: verb.stdin,
            json_output: verb.json_output,
            path_checks,
        })
    }

    /// Judge a call before it runs, without evaluating anything.
    ///
    /// A word the validation binder could not evaluate is opaque: it cannot
    /// be an unknown flag, cannot fail a `choices` set, and cannot be
    /// path-checked. A literal word is judged in full.
    pub fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        let words: Vec<Word> = args
            .positional
            .iter()
            .map(|value| Word::from_validation_text(crate::interpreter::value_to_string(value)))
            .collect();

        let call = match parse::parse(&self.declaration, &words) {
            Ok(call) => call,
            Err(failure) => return vec![issue(&failure.error, failure.uncertain)],
        };
        let Some(verb) = call.verb(&self.declaration) else {
            return Vec::new();
        };

        let mut issues: Vec<ValidationIssue> = constraint::check(&self.declaration, verb, &call)
            .iter()
            .map(|error| issue(error, call.uncertain))
            .collect();

        let rendered = render::render(&self.declaration, verb, &call);
        for check in constraint::path_checks(verb, &call, &rendered.item_argv_index) {
            if !Path::new(&check.value).is_absolute() {
                continue;
            }
            if let Err(error) = resolve_under(&check.value, Path::new("/"), &check.root) {
                issues.push(issue(
                    &error.attributed_to(&self.declaration.name, &check.positional),
                    call.uncertain,
                ));
            }
        }
        issues
    }

    /// Finish a deferred [`PathCheck`] against the kernel's real cwd.
    ///
    /// A relative value cannot be resolved when the call is planned — only
    /// the execute path knows where the kernel is. The returned path is
    /// canonical and inside the declared root; it replaces
    /// `argv[check.argv_index]` before the child runs.
    pub fn resolve_path_check(
        &self,
        check: &PathCheck,
        real_cwd: &Path,
    ) -> Result<PathBuf, WrappedError> {
        resolve_under(&check.value, real_cwd, &check.root)
            .map_err(|e| e.attributed_to(&self.declaration.name, &check.positional))
    }

    /// The words execution hands the parser: every one literal, with the two
    /// values argv cannot carry refused by name.
    fn execution_words(&self, args: &ToolArgs) -> Result<Vec<Word>, WrappedError> {
        let mut words = Vec::with_capacity(args.positional.len());
        for (offset, value) in args.positional.iter().enumerate() {
            let position = offset + 1;
            if let Value::Bytes(bytes) = value {
                return Err(WrappedError::BinaryArgument {
                    command: self.declaration.name.clone(),
                    position,
                    byte_len: bytes.len(),
                });
            }
            let text = crate::interpreter::value_to_string(value);
            if text.contains('\0') {
                return Err(WrappedError::NulByte {
                    command: self.declaration.name.clone(),
                    position,
                });
            }
            words.push(Word::literal(text));
        }
        Ok(words)
    }
}

/// Map a refusal onto the validator's vocabulary.
///
/// `uncertain` softens the verdict to a warning: a word the parser could not
/// judge sat in flag-or-positional position, so this reading of the call may
/// not be the one that runs.
fn issue(error: &WrappedError, uncertain: bool) -> ValidationIssue {
    let code = match error {
        WrappedError::UnknownFlag { .. }
        | WrappedError::ClusteredShort { .. }
        | WrappedError::GluedShortValue { .. }
        | WrappedError::UnexpectedFlagValue { .. }
        | WrappedError::RepeatedFlag { .. } => IssueCode::UnknownFlag,
        WrappedError::MissingFlagValue { .. }
        | WrappedError::MissingRequiredFlag { .. }
        | WrappedError::MissingRequiredPositional { .. } => IssueCode::MissingRequiredArg,
        WrappedError::NotAnInteger { .. } | WrappedError::NotInChoices { .. } => {
            IssueCode::InvalidArgType
        }
        _ => IssueCode::WrappedCallRejected,
    };
    let severity = if uncertain {
        Severity::Warning
    } else {
        Severity::Error
    };
    match severity {
        Severity::Warning => ValidationIssue::warning(code, error.to_string()),
        Severity::Error => ValidationIssue::error(code, error.to_string()),
    }
}

/// The schema for one named verb.
fn verb_schema(verb: &Verb) -> ToolSchema {
    let description = match verb.tail {
        Tail::Forward => append_clause(&verb.about, "forwards undeclared flags"),
        _ => verb.about.clone(),
    };
    let mut schema = ToolSchema::new(verb.name_or_root(), description);
    for param in verb_params(verb) {
        schema = schema.param(param);
    }
    for (label, command) in &verb.examples {
        schema = schema.example(label, command);
    }
    if verb.json_output {
        schema = schema.with_typed_substitution();
    }
    schema
}

/// Flags then positionals, in declaration order — the order
/// `validate_against_schema` and `help` both read.
fn verb_params(verb: &Verb) -> Vec<ParamSchema> {
    let mut params = Vec::with_capacity(verb.flags.len() + verb.positionals.len());
    for flag in &verb.flags {
        let param_type = if !flag.takes_value {
            "bool"
        } else if flag.int {
            "int"
        } else {
            "string"
        };
        let mut description = flag.about.clone();
        if !flag.choices.is_empty() {
            description = append_clause(&description, &format!("one of: {}", flag.choices.join(", ")));
        }
        let mut param = ParamSchema::new(&flag.name, param_type)
            .with_required(flag.required)
            .with_description(description)
            .with_aliases(flag.aliases.clone())
            .with_repeatable(flag.repeatable);
        if !flag.takes_value {
            param = param.with_default(Some(Value::Bool(false)));
        }
        params.push(param);
    }
    for positional in &verb.positionals {
        let mut description = positional.about.clone();
        if let Some(root) = &positional.path_under {
            description = append_clause(&description, &format!("must be under {}", root.display()));
        }
        params.push(
            ParamSchema::new(&positional.name, "string")
                .with_required(positional.required)
                .with_description(description)
                .positional(),
        );
    }
    params
}

/// Join a clause onto a description without leaving a stray separator when
/// the description is empty.
fn append_clause(description: &str, clause: &str) -> String {
    if description.is_empty() {
        clause.to_string()
    } else {
        format!("{description}; {clause}")
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests;
