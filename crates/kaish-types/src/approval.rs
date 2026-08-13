//! The statement-plan vocabulary: pure data plus serde, no behavior.
//!
//! A [`Plan`] is what `plan_program` produces for one statement — the source
//! rendered back **unexpanded**, every [`PlannedCommand`] it would run, and
//! the variables it reads and writes. An embedder reads a plan to decide
//! whether to run a statement; nothing here decides anything itself.
//!
//! [`PlannedValue`] is the one place redaction appears. The kernel redacts
//! exactly one thing — the `--confirm=<key>` flag spelling, kaish's own
//! convention for a confirmation credential — and a redacted value keeps a
//! *kind*, never the credential. kaish ships no secret detector, because a
//! shell cannot define what a secret is; an embedder that wants more redacts
//! the plans it holds.

use serde::{Deserialize, Serialize};

/// A content identity for a plan — a digest over its rendered text with any
/// presented credential stripped, so `rm x` and `rm --confirm=<key> x`
/// digest the same. The embedder computes it (e.g. SHA-256 over the
/// kernel's `strip_confirm_tokens(rendered)`); this type only carries the
/// value, so `kaish-types` stays dependency-light.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct PlanDigest(String);

impl PlanDigest {
    /// Wrap an already-computed digest.
    pub fn new(hex: impl Into<String>) -> Self {
        Self(hex.into())
    }

    /// The digest's text form.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

// ───────────────────────── Redaction ─────────────────────────

/// One value inside a rendered plan. A sink serializes `PlannedValue`, never
/// a bare `String`, so a value reaches a sink only after something decided
/// whether it was a secret.
///
/// The kernel builds every `PlannedValue` at one normalization point
/// (`kaish-kernel`'s `ast::plan::plan_statement`), before the plan reaches
/// any consumer. A consumer added later reads the same already-decided
/// values instead of re-deriving its own redaction.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlannedValue {
    /// Not judged secret. Holds the literal text, exactly as it would render
    /// on the command line.
    Plain(String),
    /// Judged secret — today only by the kernel's own confirm-key check;
    /// the original text never reaches this variant or anything built from
    /// it. The variant is the vocabulary an embedder-side redaction pass can
    /// also produce over plans it holds.
    Redacted {
        /// What kind of secret — `"confirm-key"` for the kernel's one
        /// built-in redaction. The kernel does not interpret this string.
        kind: String,
        /// Stable salted digest prefix, when the producer supplied one, so an
        /// auditor can ask "the same credential as last time?" without
        /// holding it.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        fingerprint: Option<String>,
    },
}

impl PlannedValue {
    /// Build a value the kernel judged secret.
    pub fn redacted(kind: impl Into<String>, fingerprint: Option<String>) -> Self {
        Self::Redacted {
            kind: kind.into(),
            fingerprint,
        }
    }

    /// The text a sink should show: the literal for `Plain`, or `<kind>` for
    /// `Redacted` — never the redacted content itself.
    pub fn display(&self) -> String {
        match self {
            Self::Plain(s) => s.clone(),
            Self::Redacted { kind, .. } => format!("<{kind}>"),
        }
    }

    /// Whether this value was judged secret.
    pub fn is_redacted(&self) -> bool {
        matches!(self, Self::Redacted { .. })
    }
}

// ───────────────────────── The statement plan ─────────────────────────

/// What one top-level statement was asked to run (spec §C.6).
///
/// Built from the AST after validation and **before** execution, so it is
/// parse information and never execution information: no substitution has
/// run, no redirect has been opened, no loop has taken its first iteration.
/// Nested statements — loop bodies, `if` branches, user-tool bodies — belong
/// to their enclosing top-level statement's plan and are never planned
/// separately.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Plan {
    /// The statement rendered back to shell text, **unexpanded**: `${HOME}`
    /// and `$(...)` appear as written, because a classifier judges what was
    /// asked, not what it resolved to. Truncated at
    /// [`PLAN_RENDER_LIMIT`] bytes with a marker naming the limit.
    pub rendered: String,
    /// The statement's kind: `"command"`, `"pipeline"`, `"for"`,
    /// `"and_chain"`, …
    pub statement_kind: String,
    /// Every command the statement contains, control-structure bodies
    /// included.
    pub commands: Vec<PlannedCommand>,
    /// Session variables the statement reads and does not itself lexically
    /// bind — sorted, deduplicated root names. Complete against the
    /// statement's **lexical** surface — kaish has no `eval` and no indirect
    /// expansion, so every read is visible in the source. It does not cover
    /// names bound at runtime by a builtin that takes them as arguments:
    /// `read`, `export`, `unset`, and `push` write session variables that
    /// argv-level analysis cannot see, so `read TOKEN && echo $TOKEN`
    /// reports `TOKEN` here, and the value an embedder peeks with
    /// `Kernel::get_var` is the one from before the `read`. Special forms
    /// (`$1`, `$?`, `$$`, `$@`, `$#`) are not session variables and are not
    /// listed.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub free_variables: Vec<String>,
    /// Names the statement itself binds **lexically** — an assignment
    /// target, a `for` variable, an env-prefix name, a tool-def parameter.
    /// Peeking session state for these is misleading (the statement supplies
    /// its own value), so a name that is both read and lexically bound lands
    /// here, never in `free_variables` — the safe direction. A name written
    /// only through a runtime binder (`read`, `export`, `unset`, `push`) is
    /// a plain argument, not a lexical bind: it lands in `free_variables`
    /// when the statement also reads it, and in neither set otherwise.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub bound_variables: Vec<String>,
}

/// The byte limit [`Plan::rendered`] is truncated at: 8 KiB. A statement
/// longer than this is a generated program, and a classifier that needs more
/// than 8 KiB of it is reading the wrong field — [`Plan::commands`] carries
/// the structure.
pub const PLAN_RENDER_LIMIT: usize = 8 * 1024;

impl Plan {
    /// Assemble a plan. The only constructor for this `#[non_exhaustive]`
    /// type — `rendered` is stored verbatim, so a producer truncates before
    /// calling.
    pub fn new(
        rendered: impl Into<String>,
        statement_kind: impl Into<String>,
        commands: Vec<PlannedCommand>,
    ) -> Self {
        Self {
            rendered: rendered.into(),
            statement_kind: statement_kind.into(),
            commands,
            free_variables: Vec::new(),
            bound_variables: Vec::new(),
        }
    }

    /// Attach the statement's variable analysis (sorted, deduplicated).
    pub fn with_variables(mut self, free: Vec<String>, bound: Vec<String>) -> Self {
        self.free_variables = free;
        self.bound_variables = bound;
        self
    }
}

/// One command inside a [`Plan`], as written (spec §C.6).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlannedCommand {
    /// argv0 as written — never resolved through aliases, `PATH`, or the
    /// tool registry. Never a [`PlannedValue`]: a command name is structural,
    /// never a credential.
    pub name: String,
    /// The arguments, rendered unexpanded — a presented confirm key reads as
    /// `PlannedValue::Redacted` here rather than as its literal text
    /// (spec §A.8).
    pub args: Vec<PlannedValue>,
    /// The redirections this command declares.
    pub redirects: Vec<PlannedRedirect>,
    /// Whether the enclosing pipeline was backgrounded with `&`.
    pub background: bool,
}

impl PlannedCommand {
    /// Name one planned command. The only constructor for this
    /// `#[non_exhaustive]` type.
    pub fn new(
        name: impl Into<String>,
        args: Vec<PlannedValue>,
        redirects: Vec<PlannedRedirect>,
        background: bool,
    ) -> Self {
        Self {
            name: name.into(),
            args,
            redirects,
            background,
        }
    }
}

/// One redirection inside a [`PlannedCommand`] (spec §C.6).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlannedRedirect {
    /// The operator as written: `">"`, `">>"`, `"2>"`, `"<"`, `"<<<"`, …
    pub kind: String,
    /// The target, rendered unexpanded — `> ${LOG}` keeps `${LOG}` — and
    /// through the same redaction seam every argument passes (spec §A.8).
    pub target: PlannedValue,
}

impl PlannedRedirect {
    /// Name one planned redirection. The only constructor for this
    /// `#[non_exhaustive]` type.
    pub fn new(kind: impl Into<String>, target: PlannedValue) -> Self {
        Self {
            kind: kind.into(),
            target,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_plan() -> Plan {
        Plan::new(
            "cargo build > ${LOG}",
            "command",
            vec![PlannedCommand::new(
                "cargo",
                vec![PlannedValue::Plain("build".to_string())],
                vec![PlannedRedirect::new(">", PlannedValue::Plain("${LOG}".to_string()))],
                false,
            )],
        )
    }

    #[test]
    fn a_plan_round_trips_with_every_planned_field() {
        let plan = sample_plan();
        let json = serde_json::to_value(&plan).expect("serialize");
        let back: Plan = serde_json::from_value(json).expect("deserialize");
        assert_eq!(plan, back);
        assert_eq!(back.commands[0].redirects[0].kind, ">");
        // Unexpanded: the target keeps `${LOG}` as written, because an
        // embedder judges what was asked, not what it resolved to.
        assert_eq!(
            back.commands[0].redirects[0].target,
            PlannedValue::Plain("${LOG}".to_string())
        );
    }

    #[test]
    fn variables_default_to_empty_and_survive_a_round_trip() {
        let bare = sample_plan();
        assert!(bare.free_variables.is_empty());
        assert!(bare.bound_variables.is_empty());

        let plan = sample_plan()
            .with_variables(vec!["LOG".to_string()], vec!["OUT".to_string()]);
        let json = serde_json::to_value(&plan).expect("serialize");
        let back: Plan = serde_json::from_value(json).expect("deserialize");
        assert_eq!(back.free_variables, vec!["LOG".to_string()]);
        assert_eq!(back.bound_variables, vec!["OUT".to_string()]);
    }

    #[test]
    fn a_redacted_value_keeps_no_text() {
        // The kernel redacts its own confirm key and nothing else; the
        // variant carries a kind, never the credential it replaced.
        let json = serde_json::to_value(PlannedValue::redacted("confirm-key", None))
            .expect("serialize");
        assert!(
            !json.to_string().contains("secret"),
            "a redacted value must not carry text: {json}"
        );
    }

    #[test]
    fn a_plan_digest_round_trips() {
        let digest = PlanDigest::new("abc123");
        let json = serde_json::to_string(&digest).expect("serialize");
        let back: PlanDigest = serde_json::from_str(&json).expect("deserialize");
        assert_eq!(digest, back);
    }
}
