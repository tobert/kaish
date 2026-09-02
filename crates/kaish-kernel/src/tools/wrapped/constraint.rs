//! Constraint checks: `required`, `int`, `choices`, `path_under`.
//!
//! The first three are pure. `path_under` needs the kernel's path resolution,
//! so it lives here as two functions the caller composes: [`path_is_under`]
//! judges two already-canonicalized paths, and [`resolve_under`] canonicalizes
//! a value against a real cwd first.

use std::path::{Component, Path, PathBuf};

use super::declaration::{Verb, WrappedCommand};
use super::error::WrappedError;
use super::parse::{Call, Item};
use super::PathCheck;

/// Check every constraint the parse could not: required flags and
/// positionals, `int`, and `choices`.
///
/// Returns every failure, not just the first — a validator reports them all,
/// and an executing caller takes the first.
///
/// `uncertain` suppresses the required checks: a word the parser could not
/// judge may have filled the slot that looks empty.
pub(crate) fn check(
    declaration: &WrappedCommand,
    verb: &Verb,
    call: &Call,
) -> Vec<WrappedError> {
    let scope = declaration.scope_of_path(&call.verb_path);
    let mut errors = Vec::new();

    for use_ in &call.flags {
        let Some(flag) = verb.flags.get(use_.flag_index) else {
            continue;
        };
        // A value from an expansion is opaque: it cannot satisfy a set it
        // might be in, and it cannot fail one it might not be in.
        if !use_.value_known {
            continue;
        }
        let Some(value) = use_.value.as_deref() else {
            continue;
        };
        if flag.int && value.parse::<i64>().is_err() {
            errors.push(WrappedError::NotAnInteger {
                command: declaration.name.clone(),
                flag: flag.written_name(),
                value: value.to_string(),
            });
        }
        if !flag.choices.is_empty() && !flag.choices.iter().any(|c| c == value) {
            errors.push(WrappedError::NotInChoices {
                command: declaration.name.clone(),
                flag: flag.written_name(),
                value: value.to_string(),
                choices: flag.choices.clone(),
            });
        }
    }

    if call.uncertain {
        return errors;
    }

    for (flag_index, flag) in verb.flags.iter().enumerate() {
        if flag.required && !call.flags.iter().any(|u| u.flag_index == flag_index) {
            errors.push(WrappedError::MissingRequiredFlag {
                command: declaration.name.clone(),
                scope: scope.clone(),
                flag: flag.written_name(),
            });
        }
    }

    for (slot, positional) in verb.positionals.iter().enumerate() {
        if positional.required && !call.items.iter().any(|item| filled(item, slot)) {
            errors.push(WrappedError::MissingRequiredPositional {
                command: declaration.name.clone(),
                scope: scope.clone(),
                positional: positional.name.clone(),
            });
        }
    }

    errors
}

fn filled(item: &Item, slot: usize) -> bool {
    matches!(item, Item::Positional { slot: filled, .. } if *filled == slot)
}

/// One [`PathCheck`] per word that filled a `path_under` slot, in argv order.
///
/// A word the parser could not judge is skipped: there is no path to check
/// until the value exists.
pub(crate) fn path_checks(
    verb: &Verb,
    call: &Call,
    item_argv_index: &[usize],
) -> Vec<PathCheck> {
    let mut checks = Vec::new();
    for (item_index, item) in call.items.iter().enumerate() {
        let Item::Positional { slot, value, known } = item else {
            continue;
        };
        if !known {
            continue;
        }
        let Some(positional) = verb.positionals.get(*slot) else {
            continue;
        };
        let Some(root) = &positional.path_under else {
            continue;
        };
        let Some(argv_index) = item_argv_index.get(item_index) else {
            continue;
        };
        checks.push(PathCheck {
            positional: positional.name.clone(),
            value: value.clone(),
            root: root.clone(),
            argv_index: *argv_index,
            resolved: false,
        });
    }
    checks
}

/// True when `candidate` is inside `root`, compared component by component.
///
/// Both paths must already be canonical. String prefixes are not enough:
/// `/opt/app/scripts-evil/x` shares a prefix with `/opt/app/scripts` and is
/// not under it.
pub(crate) fn path_is_under(candidate: &Path, root: &Path) -> bool {
    let mut candidate_components = candidate.components();
    for root_component in root.components() {
        match candidate_components.next() {
            Some(component) if component == root_component => {}
            _ => return false,
        }
    }
    true
}

/// Resolve `value` against `real_cwd`, follow every symlink, and check that
/// the result is inside `root`.
///
/// The returned error names the root and the value but not the declaration
/// that asked; the caller attributes it with
/// [`WrappedError::attributed_to`](super::error::WrappedError).
pub(crate) fn resolve_under(
    value: &str,
    real_cwd: &Path,
    root: &Path,
) -> Result<PathBuf, WrappedError> {
    let Ok(canonical_root) = std::fs::canonicalize(root) else {
        return Err(WrappedError::PathRootUnresolvable {
            command: String::new(),
            positional: String::new(),
            root: root.to_path_buf(),
        });
    };
    let joined = if Path::new(value).is_absolute() {
        PathBuf::from(value)
    } else {
        real_cwd.join(value)
    };
    let canonical = canonicalize_existing_prefix(&joined);
    if path_is_under(&canonical, &canonical_root) {
        Ok(canonical)
    } else {
        Err(WrappedError::PathOutsideRoot {
            command: String::new(),
            positional: String::new(),
            root: root.to_path_buf(),
            value: value.to_string(),
        })
    }
}

/// Canonicalize as much of `path` as exists, then apply what is left
/// lexically.
///
/// A path a call names need not exist yet — a script about to be written, an
/// output file. Canonicalizing the existing prefix follows every symlink that
/// is really there; a component that does not exist cannot be a symlink, so
/// applying it lexically resolves to the same place the kernel would reach.
fn canonicalize_existing_prefix(path: &Path) -> PathBuf {
    let mut existing = path.to_path_buf();
    let mut remainder: Vec<Component<'_>> = Vec::new();
    let components: Vec<Component<'_>> = path.components().collect();
    let mut keep = components.len();

    let canonical = loop {
        if let Ok(canonical) = std::fs::canonicalize(&existing) {
            break canonical;
        }
        if keep == 0 {
            // Nothing of the path exists, not even the root. Fall back to the
            // lexical reading rather than inventing a location.
            break PathBuf::new();
        }
        keep -= 1;
        remainder.push(components[keep]);
        existing = components[..keep].iter().collect();
    };

    let mut resolved = canonical;
    for component in remainder.iter().rev() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                resolved.pop();
            }
            other => resolved.push(other.as_os_str()),
        }
    }
    resolved
}
