//! Command classification — how the kernel will resolve a command name.
//!
//! `CommandKind` is the typed answer to "what will kaish actually run for this
//! name?" It exists so embedders (kaijutsu's consent gate, kaibo) can walk a
//! parsed script and bucket each command node without re-implementing kaish's
//! resolution rules. Re-deriving those rules in the embedder forks kaish's
//! command-resolution truth: the day the kernel refines how a name resolves, a
//! hand-rolled copy silently disagrees with what kaish will actually execute —
//! a security-relevant divergence for anything gating external commands.

/// The category the kernel resolves a command name into.
///
/// Returned by `Kernel::classify_command`. The classification mirrors the
/// interpreter's real resolution order (`execute_command_depth`), not the
/// validator's warning heuristics — it answers "what will run", which is what a
/// consent gate needs.
///
/// The safe direction of any imprecision is to *over*-report `External`: an
/// embedder gating external commands should never see something classified as
/// internal that in fact escapes to `PATH`.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CommandKind {
    /// A built-in tool run in-process (e.g. `cat`, `grep`, `jq`).
    Builtin,
    /// A user-defined function (`fn name { … }`) — shadows a builtin of the same
    /// name, matching the interpreter's user-tools-first resolution.
    UserTool,
    /// An interpreter special-form handled directly, never resolved through the
    /// registry or `PATH`: `true`, `false`, `source`, `.`.
    Special,
    /// The name can't be resolved statically because it's a variable or
    /// command-substitution expansion (`$cmd`, `$(pick)`). The embedder must
    /// treat it conservatively.
    Dynamic,
    /// Not a builtin, user function, or special-form: kaish will look it up as a
    /// `.kai` script or external binary on `PATH`. This is the bucket a consent
    /// gate cares about.
    External,
}

impl CommandKind {
    /// True when the name escapes the kernel to a `PATH` lookup (`External`) or
    /// can't be resolved statically (`Dynamic`) — the two cases an external-command
    /// consent gate must scrutinize.
    pub fn escapes_kernel(self) -> bool {
        matches!(self, CommandKind::External | CommandKind::Dynamic)
    }
}
