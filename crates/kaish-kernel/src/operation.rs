//! The kernel's operation taxonomy — what class of effect a builtin can have.
//!
//! This is analysis vocabulary, not policy. A tool declares the operations it
//! can perform via [`ToolSchema::with_operations`](kaish_types::ToolSchema::with_operations),
//! and an embedder reading a plan learns that `rm` can remove a path without
//! having to recognize the name `rm`. The kernel itself does not decide
//! anything from these ids.
//!
//! In-tree operations come from a closed enum and the mapping to the dotted id
//! is an exhaustive match, so a new effect site is a **compile error** until it
//! names its operation — a string sniff on the command name would instead pick
//! a plausible wrong default in silence. The `fs.` and `trash.` namespaces
//! belong to the kernel.

/// Every operation an in-tree builtin can declare.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KernelOperation {
    /// `rm` removing a path permanently — the trash did not catch it.
    FsRemove,
    /// A truncating overwrite of an existing file (`cp`, `dd`, `patch`,
    /// `sed -i`, `tee`, `write`).
    FsOverwrite,
    /// `mv` replacing an existing destination.
    FsRename,
    /// `kaish-trash empty` discarding the recovery net itself.
    TrashEmpty,
}

impl KernelOperation {
    /// The dotted id this operation declares. Exhaustive by construction.
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::FsRemove => "fs.remove",
            Self::FsOverwrite => "fs.overwrite",
            Self::FsRename => "fs.rename",
            Self::TrashEmpty => "trash.empty",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_operation_has_a_dotted_two_part_id() {
        for op in [
            KernelOperation::FsRemove,
            KernelOperation::FsOverwrite,
            KernelOperation::FsRename,
            KernelOperation::TrashEmpty,
        ] {
            let id = op.as_str();
            let parts: Vec<&str> = id.split('.').collect();
            assert_eq!(parts.len(), 2, "{id} must be `namespace.verb`");
            assert!(
                parts.iter().all(|p| !p.is_empty()),
                "{id} has an empty half"
            );
        }
    }

    #[test]
    fn ids_are_distinct() {
        let ids = [
            KernelOperation::FsRemove.as_str(),
            KernelOperation::FsOverwrite.as_str(),
            KernelOperation::FsRename.as_str(),
            KernelOperation::TrashEmpty.as_str(),
        ];
        let unique: std::collections::BTreeSet<_> = ids.iter().collect();
        assert_eq!(unique.len(), ids.len(), "two operations share a dotted id");
    }
}
