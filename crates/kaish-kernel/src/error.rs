//! Public error type for the kernel's execute surface.
//!
//! An embedder reported the consequence of collapsing every `execute`
//! failure into one untyped `anyhow::Error`: it could not tell a validator
//! rejection ("you wrote a bad command") from a genuine execution fault
//! ("something broke while running"), so it routed both identically —
//! surfacing a syntax hint to a model as if the shell itself had crashed.
//! [`KernelError`] carries the distinction the kernel's own control flow
//! already makes, so a caller can match on it instead of parsing the
//! message text.

use crate::parser::ParseError;
use crate::validator::ValidationIssue;

/// Why a call to [`crate::kernel::Kernel::execute`] (or a sibling —
/// `execute_with_options`, `execute_argv`, …) did not return a result.
///
/// The essential distinction is **rejected before running** versus **failed
/// while running**. [`KernelError::is_rejected`] answers that question
/// without inspecting `Display` text; match on the variant for the finer
/// detail each rejection carries (issue codes, spans, parse locations).
///
/// - [`KernelError::Parse`] and [`KernelError::Validation`] are rejections:
///   the kernel never ran a statement.
/// - [`KernelError::Execution`] means a statement started running and
///   something failed partway through — a builtin, the evaluator, an IO
///   fault, or any other error the interpreter propagated.
///
/// `Display` on every variant reproduces exactly what `Kernel::execute`
/// returned before this type existed, so a caller that only prints the
/// error sees no change on upgrade.
///
/// `#[non_exhaustive]`: a rejection reason can be added later — a refused
/// external command is one candidate under discussion — without breaking a
/// caller that already has a wildcard arm. Add a match arm rather than
/// expect this list to stay closed.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum KernelError {
    /// The input did not lex or parse. Nothing ran.
    #[error("{message}")]
    Parse {
        /// Every lex/parse failure found, in source order. Each carries its
        /// own span; `errors[i].format(source)` reproduces one line of
        /// `message`.
        errors: Vec<ParseError>,
        /// Pre-rendered `"parse error:\n..."` text, byte-identical to what
        /// `Kernel::execute` returned as an `anyhow::Error` before this type
        /// existed.
        message: String,
    },

    /// The pre-execution validator rejected the program. Nothing ran.
    #[error("{message}")]
    Validation {
        /// Every error-severity issue the validator raised, carrying its
        /// [`kaish_tool_api::IssueCode`](crate::validator::IssueCode),
        /// message, and span. Warnings never appear here — they don't
        /// prevent execution, so they surface on the successful `ExecResult`
        /// instead.
        issues: Vec<ValidationIssue>,
        /// Pre-rendered `"validation failed:\n..."` text, byte-identical to
        /// what `Kernel::execute` returned as an `anyhow::Error` before this
        /// type existed.
        message: String,
    },

    /// A statement started running and something failed partway through —
    /// a builtin, the evaluator, dispatch, or an IO fault. Carries the
    /// original error chain unchanged; `source()` and `{:#}` still walk it.
    #[error("{0}")]
    Execution(anyhow::Error),
}

impl KernelError {
    /// The program was rejected before anything ran — a lex/parse failure
    /// ([`KernelError::Parse`]) or a validator rejection
    /// ([`KernelError::Validation`]). No statement executed, so nothing an
    /// embedder retries would have a different partial side effect.
    pub fn is_rejected(&self) -> bool {
        matches!(self, KernelError::Parse { .. } | KernelError::Validation { .. })
    }

    /// A statement began running and faulted partway through
    /// ([`KernelError::Execution`]). The complement of [`Self::is_rejected`].
    pub fn is_execution_failure(&self) -> bool {
        matches!(self, KernelError::Execution(_))
    }
}

/// Classify the `anyhow::Error` the internal `run_inner`/`execute_argv_locked`
/// call chain returns into the public [`KernelError`].
///
/// `Kernel::execute_streaming_inner` tags its two rejection sites (parse,
/// validation) by boxing a `KernelError` into the `anyhow::Error` it returns;
/// everything else it and the deeper interpreter propagate (`?` through
/// `execute_stmt_flow`, `eval_expr_async`, dispatch, tool bodies, …) stays a
/// plain `anyhow::Error`, untouched. This is the one place that downcasts: it
/// recovers a tagged rejection when the chain carries one, and falls back to
/// [`KernelError::Execution`] for everything else. Every public `execute*`
/// method applies this at its own return, so the interpreter's internal
/// `Result<T>` (`anyhow::Result`) never has to change shape.
pub(crate) fn classify_execute_error(e: anyhow::Error) -> KernelError {
    match e.downcast::<KernelError>() {
        Ok(tagged) => tagged,
        Err(e) => KernelError::Execution(e),
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;

    #[test]
    fn is_rejected_true_for_parse_and_validation() {
        let parse = KernelError::Parse { errors: Vec::new(), message: "parse error:\nx".into() };
        assert!(parse.is_rejected());
        assert!(!parse.is_execution_failure());

        let validation = KernelError::Validation { issues: Vec::new(), message: "validation failed:\nx".into() };
        assert!(validation.is_rejected());
        assert!(!validation.is_execution_failure());
    }

    #[test]
    fn is_rejected_false_for_execution() {
        let exec = KernelError::Execution(anyhow::anyhow!("boom"));
        assert!(!exec.is_rejected());
        assert!(exec.is_execution_failure());
    }

    #[test]
    fn classify_recovers_a_tagged_rejection() {
        let tagged = KernelError::Validation { issues: Vec::new(), message: "validation failed:\nx".into() };
        let boxed = anyhow::Error::from(tagged);
        let classified = classify_execute_error(boxed);
        assert!(classified.is_rejected());
    }

    #[test]
    fn classify_falls_back_to_execution_for_untagged_errors() {
        let classified = classify_execute_error(anyhow::anyhow!("some deep interpreter error"));
        assert!(matches!(classified, KernelError::Execution(_)));
    }

    #[test]
    fn anyhow_error_from_kernel_error_works() {
        // An embedder that wants to keep using anyhow can still do so —
        // `?` converts via anyhow's blanket `From<E: std::error::Error>`.
        fn as_anyhow() -> anyhow::Result<()> {
            fn fails() -> Result<(), KernelError> {
                Err(KernelError::Execution(anyhow::anyhow!("boom")))
            }
            fails()?;
            Ok(())
        }
        assert!(as_anyhow().is_err());
    }
}
