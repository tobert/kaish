//! Errors a wrapped command's declaration raises against a call.
//!
//! Every variant is a refusal to run: the declaration does not describe the
//! call, so nothing spawns. All of them exit 2 ([`WrappedError::exit_code`]).
//! `Display` renders the whole line the agent sees, command prefix included,
//! so a caller writes the message straight to stderr.

use std::path::PathBuf;

/// A call the declaration refuses.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[non_exhaustive]
pub enum WrappedError {
    /// The first word names no declared verb.
    #[error("{command}: unknown verb '{word}'. Allowed: {}", allowed_list(allowed))]
    UnknownVerb {
        /// The wrapped command's name.
        command: String,
        /// The word that named no verb.
        word: String,
        /// Every declared verb name, sorted.
        allowed: Vec<String>,
    },

    /// The command declares verbs and no root, and the call named none.
    #[error("{command}: no verb given. Allowed: {}", allowed_list(allowed))]
    MissingVerb {
        /// The wrapped command's name.
        command: String,
        /// Every declared verb name, sorted.
        allowed: Vec<String>,
    },

    /// A word in flag position names no declared flag or alias.
    #[error("{command}: unknown flag '{word}' for '{scope}'. Allowed: {}", allowed_list(allowed))]
    UnknownFlag {
        /// The wrapped command's name.
        command: String,
        /// `git log` for a named verb, `python` for the root verb.
        scope: String,
        /// The word that named no flag.
        word: String,
        /// Every declared flag, sorted by name and spelled `-n/--max-count`.
        allowed: Vec<String>,
    },

    /// Short flags were written as one word (`-sv`).
    #[error("{command}: '{word}' is not a flag for '{scope}'. Use {separated}.")]
    ClusteredShort {
        /// The wrapped command's name.
        command: String,
        /// `git status` for a named verb, `python` for the root verb.
        scope: String,
        /// The word as written.
        word: String,
        /// The same flags as separate words (`-s -v`).
        separated: String,
    },

    /// A short flag and its value were written as one word (`-n5`).
    #[error("{command}: '{word}' is not a flag for '{scope}'. Use {separated}.")]
    GluedShortValue {
        /// The wrapped command's name.
        command: String,
        /// `git log` for a named verb, `python` for the root verb.
        scope: String,
        /// The word as written.
        word: String,
        /// The flag and its value as separate words (`-n 5`).
        separated: String,
    },

    /// A value flag ended the argv with nothing to bind.
    #[error("{command}: '{flag}' needs a value for '{scope}'.")]
    MissingFlagValue {
        /// The wrapped command's name.
        command: String,
        /// `git log` for a named verb, `python` for the root verb.
        scope: String,
        /// The flag as written.
        flag: String,
    },

    /// A switch was written with an attached value (`--oneline=1`).
    #[error("{command}: '{flag}' takes no value for '{scope}'.")]
    UnexpectedFlagValue {
        /// The wrapped command's name.
        command: String,
        /// `git log` for a named verb, `python` for the root verb.
        scope: String,
        /// The flag as written.
        flag: String,
    },

    /// A flag the declaration did not mark repeatable appeared twice.
    #[error("{command}: '{flag}' given more than once for '{scope}'.")]
    RepeatedFlag {
        /// The wrapped command's name.
        command: String,
        /// `git log` for a named verb, `python` for the root verb.
        scope: String,
        /// The flag's declared name, written as the child sees it.
        flag: String,
    },

    /// A word filled no declared slot and the verb denies a tail.
    #[error("{command}: unexpected argument '{word}'")]
    UnexpectedArgument {
        /// The wrapped command's name.
        command: String,
        /// The word that filled no slot.
        word: String,
    },

    /// A word filled no declared slot, and the verb takes undescribed argv
    /// only past the `--` the agent writes.
    #[error("{command}: unexpected argument '{word}' for '{scope}'. Write -- before arguments meant for the program.")]
    UndeclaredPositional {
        /// The wrapped command's name.
        command: String,
        /// `cargo test` for a named verb, `python` for the root verb.
        scope: String,
        /// The word that filled no slot.
        word: String,
    },

    /// A required flag was absent.
    #[error("{command}: required flag '{flag}' not given for '{scope}'.")]
    MissingRequiredFlag {
        /// The wrapped command's name.
        command: String,
        /// `git commit` for a named verb, `python` for the root verb.
        scope: String,
        /// The flag's declared name, written as the child sees it.
        flag: String,
    },

    /// A required positional was absent.
    #[error("{command}: required argument '{positional}' not given for '{scope}'.")]
    MissingRequiredPositional {
        /// The wrapped command's name.
        command: String,
        /// `git push` for a named verb, `python` for the root verb.
        scope: String,
        /// The positional's declared name.
        positional: String,
    },

    /// An `int()` flag's value does not parse as an `i64`.
    #[error("{command}: '{flag}' takes an integer. Got '{value}'.")]
    NotAnInteger {
        /// The wrapped command's name.
        command: String,
        /// The flag's declared name, written as the child sees it.
        flag: String,
        /// The value as written.
        value: String,
    },

    /// A `choices()` flag's value is outside the set.
    #[error("{command}: '{flag}' must be one of: {}. Got '{value}'.", choices.join(", "))]
    NotInChoices {
        /// The wrapped command's name.
        command: String,
        /// The flag's declared name, written as the child sees it.
        flag: String,
        /// The value as written.
        value: String,
        /// The declared set, in declaration order.
        choices: Vec<String>,
    },

    /// A `path_under()` positional resolved outside its root.
    #[error("{command}: '{positional}' must be under {}. Got '{value}'.", root.display())]
    PathOutsideRoot {
        /// The wrapped command's name.
        command: String,
        /// The positional's declared name.
        positional: String,
        /// The declared root.
        root: PathBuf,
        /// The value as written.
        value: String,
    },

    /// A `path_under()` root does not resolve on the real filesystem, so
    /// containment cannot be proven either way.
    #[error("{command}: '{positional}' must be under {}, which does not resolve.", root.display())]
    PathRootUnresolvable {
        /// The wrapped command's name.
        command: String,
        /// The positional's declared name.
        positional: String,
        /// The declared root.
        root: PathBuf,
    },

    /// A word carries a NUL byte, which no child's argv can hold.
    ///
    /// The declaration refuses the call here rather than at spawn: the parse
    /// runs before anything is created, and a refusal that early cannot
    /// truncate the value on its way to the child.
    #[error("{command}: argument {position} contains a NUL byte. argv cannot carry NUL; remove it.")]
    NulByte {
        /// The wrapped command's name.
        command: String,
        /// 1-based index among the words the tool received.
        position: usize,
    },

    /// A word is binary, which does not cross the argv text boundary.
    #[error("{command}: argument {position} is binary ({byte_len} bytes). argv carries text; encode it or write it to a file.")]
    BinaryArgument {
        /// The wrapped command's name.
        command: String,
        /// 1-based index among the words the tool received.
        position: usize,
        /// How many bytes it held. Never the bytes themselves.
        byte_len: usize,
    },
}

impl WrappedError {
    /// Exit code for every refusal: 2, the shell's usage-error code. Nothing
    /// spawned, so the child's own codes are not in play.
    pub fn exit_code(&self) -> i64 {
        2
    }

    /// Fill in the command and positional a bare path failure belongs to.
    ///
    /// [`crate::tools::wrapped::path_is_under`]'s companion
    /// [`crate::tools::wrapped::resolve_under`] is a general helper: it knows
    /// the value and the root, not which declaration asked. The caller
    /// attributes the failure before it reaches an agent.
    pub(crate) fn attributed_to(mut self, command: &str, positional: &str) -> Self {
        match &mut self {
            WrappedError::PathOutsideRoot {
                command: c,
                positional: p,
                ..
            }
            | WrappedError::PathRootUnresolvable {
                command: c,
                positional: p,
                ..
            } => {
                c.clear();
                c.push_str(command);
                p.clear();
                p.push_str(positional);
            }
            _ => {}
        }
        self
    }
}

/// Render an allowed set for an error message: comma-joined, or `(none)` when
/// the declaration allows nothing here. Deny-by-default makes the empty set
/// common enough that it needs a spelling of its own.
fn allowed_list(allowed: &[String]) -> String {
    if allowed.is_empty() {
        "(none)".to_string()
    } else {
        allowed.join(", ")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unknown_verb_reads_as_the_declaration_documents() {
        let error = WrappedError::UnknownVerb {
            command: "git".into(),
            word: "comit".into(),
            allowed: vec![
                "commit".into(),
                "diff".into(),
                "log".into(),
                "push".into(),
                "status".into(),
            ],
        };
        assert_eq!(
            error.to_string(),
            "git: unknown verb 'comit'. Allowed: commit, diff, log, push, status"
        );
    }

    #[test]
    fn unknown_flag_names_the_scope_and_the_allowed_set() {
        let error = WrappedError::UnknownFlag {
            command: "git".into(),
            scope: "git log".into(),
            word: "--output".into(),
            allowed: vec!["-n/--max-count".into(), "--oneline".into(), "--since".into()],
        };
        assert_eq!(
            error.to_string(),
            "git: unknown flag '--output' for 'git log'. Allowed: -n/--max-count, --oneline, --since"
        );
    }

    #[test]
    fn an_empty_allowed_set_reads_as_none() {
        let error = WrappedError::UnknownFlag {
            command: "python".into(),
            scope: "python".into(),
            word: "-c".into(),
            allowed: Vec::new(),
        };
        assert_eq!(
            error.to_string(),
            "python: unknown flag '-c' for 'python'. Allowed: (none)"
        );
    }

    #[test]
    fn every_refusal_exits_two() {
        let error = WrappedError::UnexpectedArgument {
            command: "cargo".into(),
            word: "extra".into(),
        };
        assert_eq!(error.exit_code(), 2);
        assert_eq!(error.to_string(), "cargo: unexpected argument 'extra'");
    }

    #[test]
    fn attribution_fills_a_bare_path_failure() {
        let error = WrappedError::PathOutsideRoot {
            command: String::new(),
            positional: String::new(),
            root: PathBuf::from("/opt/app/scripts"),
            value: "/etc/passwd".into(),
        }
        .attributed_to("python", "script");
        assert_eq!(
            error.to_string(),
            "python: 'script' must be under /opt/app/scripts. Got '/etc/passwd'."
        );
    }
}
