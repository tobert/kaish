//! Parsing a raw-argv word list against a declaration.
//!
//! The kernel hands a wrapped command its argv in source order with `--`
//! preserved (`ToolSchema::raw_argv`). This module reads that list. There is
//! no clap layer, no prefix matching, no clustered shorts, and no glued short
//! values — a word matches a declared name or alias exactly, or it does not
//! match.
//!
//! # Words the validator cannot evaluate
//!
//! The same parser serves validation, where a `$var` or `$(cmd)` word arrives
//! as a placeholder rather than its value. [`Known`] carries that: an opaque
//! word is never judged, and — because an opaque word in flag-or-positional
//! position could have been a value flag that ate the next word — everything
//! after it is reported as [`Call::uncertain`] so the caller can soften its
//! verdict.

use super::declaration::{Positional, Tail, Verb, WrappedCommand};
use super::error::WrappedError;

/// The placeholder the validation binder writes for a word it cannot
/// evaluate. See `build_tool_args_for_validation` in the validator.
const DYNAMIC: &str = "<dynamic>";

/// How much of a word the parser is allowed to judge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Known {
    /// Every character came from the source.
    Literal,
    /// The word came from an expansion; nothing about it is known.
    Opaque,
    /// `--name=` came from the source; the text after `=` did not.
    OpaqueValue,
}

/// One argv word, with how much of it the parser may judge.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Word {
    /// The word as the child would see it, or the placeholder standing for it.
    pub(crate) text: String,
    /// How much of `text` came from the source.
    pub(crate) known: Known,
}

impl Word {
    /// A word every character of which came from the source.
    pub(crate) fn literal(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            known: Known::Literal,
        }
    }

    /// A word as the validation binder wrote it: `<dynamic>` for a value it
    /// could not evaluate, `--name=<dynamic>` for a long flag whose value it
    /// could not evaluate, and the literal text for everything else.
    pub(crate) fn from_validation_text(text: impl Into<String>) -> Self {
        let text = text.into();
        let known = if text == DYNAMIC {
            Known::Opaque
        } else if text.starts_with("--") && text.ends_with(&format!("={DYNAMIC}")) {
            Known::OpaqueValue
        } else {
            Known::Literal
        };
        Self { text, known }
    }
}

/// One occurrence of a declared flag, in source order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FlagUse {
    /// Index into the selected verb's `flags`.
    pub(crate) flag_index: usize,
    /// The bound value, `None` for a switch.
    pub(crate) value: Option<String>,
    /// False when the value came from an expansion the parser cannot judge.
    pub(crate) value_known: bool,
}

/// One word of the call, in the order the agent wrote it.
///
/// Every word the child sees is here, so rendering is a walk over this list:
/// the declaration decides how a word is spelled, never where it sits.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Item {
    /// A word that filled a declared slot.
    Positional {
        /// Index into the selected verb's `positionals`.
        slot: usize,
        /// The word as written.
        value: String,
        /// False when the word came from an expansion.
        known: bool,
    },
    /// One declared flag occurrence: an index into [`Call::flags`].
    Flag(usize),
    /// A `--` the agent wrote. It renders where it was written.
    DashDash,
    /// A word the declaration does not describe, forwarded verbatim: an
    /// undeclared flag under `Tail::Forward`, or a word past every declared
    /// slot that this verb's [`Tail`] lets through.
    Undeclared(String),
}

/// A parsed call, ready to render.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Call {
    /// The path from the top down to the selected leaf: each entry is an
    /// index into the previous level's `verbs` (the first into
    /// `declaration.verbs`, every one after into the previous verb's own
    /// `verbs`). Empty selects the root verb. The root can never be a node
    /// (`build()` refuses that), so an empty path always names a leaf.
    pub(crate) verb_path: Vec<usize>,
    /// Declared flag occurrences, in source order. `items` points into this.
    pub(crate) flags: Vec<FlagUse>,
    /// Every word after the verb, in source order.
    pub(crate) items: Vec<Item>,
    /// A word the parser could not judge landed in flag-or-positional
    /// position, so the shape after it is a guess.
    pub(crate) uncertain: bool,
}

impl Call {
    /// A call the parser declines to describe: the verb itself was opaque.
    fn unjudgeable() -> Self {
        Self {
            verb_path: Vec::new(),
            flags: Vec::new(),
            items: Vec::new(),
            uncertain: true,
        }
    }

    /// The selected verb.
    pub(crate) fn verb<'d>(&self, declaration: &'d WrappedCommand) -> Option<&'d Verb> {
        resolve_verb(declaration, &self.verb_path)
    }
}

/// Walk `path` from the declaration's top-level `verbs` down through each
/// verb's own `verbs`. An empty path selects the root verb.
fn resolve_verb<'d>(declaration: &'d WrappedCommand, path: &[usize]) -> Option<&'d Verb> {
    let Some((&first, rest)) = path.split_first() else {
        return declaration.root.as_ref();
    };
    let mut verb = declaration.verbs.get(first)?;
    for &index in rest {
        verb = verb.verbs.get(index)?;
    }
    Some(verb)
}

/// A refusal, plus whether an unjudgeable word preceded it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ParseError {
    /// The refusal itself.
    pub(crate) error: WrappedError,
    /// True when the parser had already met a word it could not judge, so the
    /// refusal may describe a shape the call will not have at runtime.
    pub(crate) uncertain: bool,
}

/// Parse `words` against `declaration`.
pub(crate) fn parse(declaration: &WrappedCommand, words: &[Word]) -> Result<Call, ParseError> {
    let Some((verb_path, mut index)) = select_verb(declaration, words)? else {
        return Ok(Call::unjudgeable());
    };
    let Some(verb) = resolve_verb(declaration, &verb_path) else {
        return Ok(Call::unjudgeable());
    };
    let scope = declaration.scope_of_path(&verb_path);

    let mut call = Call {
        verb_path,
        flags: Vec::new(),
        items: Vec::new(),
        uncertain: false,
    };
    let mut past_dash_dash = false;
    let mut slot = 0usize;

    // One closure would need the whole `call` borrowed twice; a macro keeps
    // the three call sites from drifting instead.
    macro_rules! refuse {
        ($result:expr) => {
            if let Err(error) = $result {
                return Err(ParseError {
                    error,
                    uncertain: call.uncertain,
                });
            }
        };
    }

    while index < words.len() {
        let word = &words[index];
        index += 1;

        if past_dash_dash {
            refuse!(fill_positional(
                declaration,
                verb,
                &scope,
                &mut call,
                &mut slot,
                &word.text,
                word.known,
                true,
            ));
            continue;
        }

        match word.known {
            // Nothing about the word is known, so it cannot be judged as a
            // flag. Treat it as filling the next slot — the reading that
            // reports no error — and mark the rest of the parse a guess.
            Known::Opaque => {
                call.uncertain = true;
                refuse!(fill_positional(
                    declaration,
                    verb,
                    &scope,
                    &mut call,
                    &mut slot,
                    &word.text,
                    word.known,
                    false,
                ));
                continue;
            }
            Known::Literal | Known::OpaqueValue => {}
        }

        if word.text == "--" {
            past_dash_dash = true;
            call.items.push(Item::DashDash);
            continue;
        }

        if word.text.starts_with('-') {
            refuse!(bind_flag(
                declaration,
                verb,
                &scope,
                &mut call,
                words,
                &mut index,
                word,
            ));
            continue;
        }

        refuse!(fill_positional(
            declaration,
            verb,
            &scope,
            &mut call,
            &mut slot,
            &word.text,
            word.known,
            false,
        ));
    }

    Ok(call)
}

/// Choose the verb and report how many words it consumed.
/// `Ok(None)` means the verb itself came from an expansion, so no reading of
/// the rest of the words is worth reporting.
///
/// The first word selects a top-level verb, or falls through to the root
/// exactly as a flat declaration always has. From there, [`descend`] keeps
/// consuming words for as long as the selected verb has children — a node
/// is never itself callable, so the next word must select one of them.
fn select_verb(
    declaration: &WrappedCommand,
    words: &[Word],
) -> Result<Option<(Vec<usize>, usize)>, ParseError> {
    if declaration.verbs.is_empty() {
        return Ok(Some((Vec::new(), 0)));
    }
    let allowed = allowed_verbs(declaration);
    let missing = |uncertain: bool| ParseError {
        error: WrappedError::MissingVerb {
            command: declaration.name.clone(),
            allowed: allowed.clone(),
        },
        uncertain,
    };

    let Some(first) = words.first() else {
        return match declaration.root {
            Some(_) => Ok(Some((Vec::new(), 0))),
            None => Err(missing(false)),
        };
    };

    match first.known {
        Known::Literal if !first.text.starts_with('-') => {
            match declaration
                .verbs
                .iter()
                .position(|v| v.name_or_root() == first.text)
            {
                Some(index) => {
                    let verb = &declaration.verbs[index];
                    let scope = declaration.scope_of(verb);
                    descend(declaration, scope, verb, vec![index], words, 1)
                }
                // A declaration with both a root and named verbs falls
                // through: `python etl.py` runs the root, `python json-tool`
                // runs the verb.
                None if declaration.root.is_some() => Ok(Some((Vec::new(), 0))),
                None => Err(ParseError {
                    error: WrappedError::UnknownVerb {
                        command: declaration.name.clone(),
                        word: first.text.clone(),
                        allowed,
                    },
                    uncertain: false,
                }),
            }
        }
        // A flag or `--` where a verb belongs: the root absorbs it if there
        // is one, otherwise no verb was named.
        Known::Literal => match declaration.root {
            Some(_) => Ok(Some((Vec::new(), 0))),
            None => Err(missing(false)),
        },
        // The verb itself came from an expansion. Which verb's flags apply is
        // unknowable, so the parser describes nothing.
        Known::Opaque | Known::OpaqueValue => match declaration.root {
            Some(_) => Ok(Some((Vec::new(), 0))),
            None => Ok(None),
        },
    }
}

/// After `verb` (at `path`, having consumed `consumed` words) is selected,
/// keep consuming while it has children. A node is never itself callable:
/// the word at `words[consumed]` must select one of them, or the call
/// refuses (nothing there, or a flag/`--` where a child belongs) or — for an
/// opaque word — describes nothing, the same rule a flat declaration already
/// applies at the top level.
fn descend(
    declaration: &WrappedCommand,
    scope: String,
    verb: &Verb,
    path: Vec<usize>,
    words: &[Word],
    consumed: usize,
) -> Result<Option<(Vec<usize>, usize)>, ParseError> {
    if verb.verbs.is_empty() {
        return Ok(Some((path, consumed)));
    }
    let allowed = allowed_verb_names(&verb.verbs);
    let bare_node = || ParseError {
        error: WrappedError::BareNode {
            command: declaration.name.clone(),
            scope: scope.clone(),
            allowed: allowed.clone(),
        },
        uncertain: false,
    };

    let Some(next) = words.get(consumed) else {
        return Err(bare_node());
    };

    match next.known {
        Known::Literal if !next.text.starts_with('-') => {
            match verb.verbs.iter().position(|v| v.name_or_root() == next.text) {
                Some(index) => {
                    let child = &verb.verbs[index];
                    let child_scope = format!("{scope} {}", next.text);
                    let mut child_path = path;
                    child_path.push(index);
                    descend(declaration, child_scope, child, child_path, words, consumed + 1)
                }
                None => Err(ParseError {
                    error: WrappedError::UnknownChildVerb {
                        command: declaration.name.clone(),
                        scope,
                        word: next.text.clone(),
                        allowed,
                    },
                    uncertain: false,
                }),
            }
        }
        // A flag or `--` where a child verb belongs: a node has no root to
        // fall through to, so this is the same refusal as no word at all.
        Known::Literal => Err(bare_node()),
        // The child came from an expansion. Which child's grammar applies is
        // unknowable, so the parser describes nothing — the node-depth
        // reading of the rule `select_verb` already applies at the top.
        Known::Opaque | Known::OpaqueValue => Ok(None),
    }
}

/// Bind one word in flag position.
fn bind_flag(
    declaration: &WrappedCommand,
    verb: &Verb,
    scope: &str,
    call: &mut Call,
    words: &[Word],
    index: &mut usize,
    word: &Word,
) -> Result<(), WrappedError> {
    // `--name=value` splits; a short flag never does, so `-n=5` stays one
    // word and fails the glued-value check below.
    let (head, inline) = match word.text.strip_prefix("--").and_then(|rest| {
        rest.find('=')
            .map(|eq| (&word.text[..eq + 2], word.text[eq + 3..].to_string()))
    }) {
        Some((head, value)) => (head.to_string(), Some(value)),
        None => (word.text.clone(), None),
    };

    let Some(flag_index) = verb
        .flags
        .iter()
        .position(|flag| flag.matches(&head))
    else {
        if verb.tail == Tail::Forward {
            call.items.push(Item::Undeclared(word.text.clone()));
            return Ok(());
        }
        return Err(unknown_flag(declaration, verb, scope, &word.text, &head));
    };
    let flag = &verb.flags[flag_index];

    if !flag.repeatable && call.flags.iter().any(|use_| use_.flag_index == flag_index) {
        return Err(WrappedError::RepeatedFlag {
            command: declaration.name.clone(),
            scope: scope.to_string(),
            flag: flag.written_name(),
        });
    }

    if !flag.takes_value {
        if inline.is_some() {
            return Err(WrappedError::UnexpectedFlagValue {
                command: declaration.name.clone(),
                scope: scope.to_string(),
                flag: head,
            });
        }
        call.items.push(Item::Flag(call.flags.len()));
        call.flags.push(FlagUse {
            flag_index,
            value: None,
            value_known: true,
        });
        return Ok(());
    }

    let (value, value_known) = match inline {
        Some(value) => (value, word.known == Known::Literal),
        None => {
            // A value flag takes the next word whatever it looks like:
            // `git commit -m -foo` is the message `-foo`.
            let Some(next) = words.get(*index) else {
                return Err(WrappedError::MissingFlagValue {
                    command: declaration.name.clone(),
                    scope: scope.to_string(),
                    flag: head,
                });
            };
            *index += 1;
            (next.text.clone(), next.known == Known::Literal)
        }
    };
    call.items.push(Item::Flag(call.flags.len()));
    call.flags.push(FlagUse {
        flag_index,
        value: Some(value),
        value_known,
    });
    Ok(())
}

/// Build the unknown-flag refusal, upgrading it to the clustered-shorts or
/// glued-value form when the word is one of those two spellings.
/// `word` is the whole argv word; `head` is the flag it names, which for
/// `--output=/tmp/x` is `--output`. The refusal names the flag, since that is
/// what the declaration does not have.
fn unknown_flag(
    declaration: &WrappedCommand,
    verb: &Verb,
    scope: &str,
    word: &str,
    head: &str,
) -> WrappedError {
    if let Some(separated) = clustered_shorts(verb, word) {
        return WrappedError::ClusteredShort {
            command: declaration.name.clone(),
            scope: scope.to_string(),
            word: word.to_string(),
            separated,
        };
    }
    if let Some(separated) = glued_short_value(verb, word) {
        return WrappedError::GluedShortValue {
            command: declaration.name.clone(),
            scope: scope.to_string(),
            word: word.to_string(),
            separated,
        };
    }
    WrappedError::UnknownFlag {
        command: declaration.name.clone(),
        scope: scope.to_string(),
        word: head.to_string(),
        allowed: allowed_flags(verb),
    }
}

/// `-sv` where `-s` and `-v` are both declared switches → `-s -v`.
fn clustered_shorts(verb: &Verb, word: &str) -> Option<String> {
    let rest = word.strip_prefix('-')?;
    if rest.starts_with('-') || rest.chars().count() < 2 {
        return None;
    }
    let mut separated = Vec::new();
    for ch in rest.chars() {
        let short = format!("-{ch}");
        let flag = verb
            .flags
            .iter()
            .find(|flag| flag.matches(&short))?;
        if flag.takes_value {
            return None;
        }
        separated.push(short);
    }
    Some(separated.join(" "))
}

/// `-n5` where `-n` is a declared value flag → `-n 5`.
fn glued_short_value(verb: &Verb, word: &str) -> Option<String> {
    let rest = word.strip_prefix('-')?;
    if rest.starts_with('-') || rest.chars().count() < 2 {
        return None;
    }
    let mut chars = rest.chars();
    let first = chars.next()?;
    let short = format!("-{first}");
    let flag = verb
        .flags
        .iter()
        .find(|flag| flag.matches(&short))?;
    if !flag.takes_value {
        return None;
    }
    Some(format!("{short} {}", chars.as_str()))
}

/// Put a word in the next declared slot. A `many` slot absorbs the rest.
///
/// A word past every declared slot is undescribed argv, and [`Tail`] decides
/// it. `Forward` takes it wherever it sits. `AfterDashDash` takes it only past
/// the agent's own `--`, and before that names the `--` as the fix. `Deny`
/// refuses it either way, so it is told nothing about a `--` that would not
/// help.
#[allow(clippy::too_many_arguments)]
fn fill_positional(
    declaration: &WrappedCommand,
    verb: &Verb,
    scope: &str,
    call: &mut Call,
    slot: &mut usize,
    text: &str,
    known: Known,
    past_dash_dash: bool,
) -> Result<(), WrappedError> {
    if let Some(Positional { many, .. }) = verb.positionals.get(*slot) {
        call.items.push(Item::Positional {
            slot: *slot,
            value: text.to_string(),
            known: known == Known::Literal,
        });
        if !many {
            *slot += 1;
        }
        return Ok(());
    }

    match (verb.tail, past_dash_dash) {
        (Tail::Forward, _) | (Tail::AfterDashDash, true) => {
            call.items.push(Item::Undeclared(text.to_string()));
            Ok(())
        }
        (Tail::AfterDashDash, false) => Err(WrappedError::UndeclaredPositional {
            command: declaration.name.clone(),
            scope: scope.to_string(),
            word: text.to_string(),
        }),
        (Tail::Deny, _) => Err(WrappedError::UnexpectedArgument {
            command: declaration.name.clone(),
            word: text.to_string(),
        }),
    }
}

/// Every declared top-level verb name, sorted.
pub(crate) fn allowed_verbs(declaration: &WrappedCommand) -> Vec<String> {
    allowed_verb_names(&declaration.verbs)
}

/// Every verb name in `verbs`, sorted — the siblings at one level, never the
/// top level unless `verbs` is `declaration.verbs` itself.
fn allowed_verb_names(verbs: &[Verb]) -> Vec<String> {
    let mut allowed: Vec<String> = verbs.iter().map(|verb| verb.name_or_root().to_string()).collect();
    allowed.sort();
    allowed
}

/// Every declared flag, sorted by name and spelled `-n/--max-count`.
pub(crate) fn allowed_flags(verb: &Verb) -> Vec<String> {
    let mut flags: Vec<&super::declaration::Flag> = verb.flags.iter().collect();
    flags.sort_by(|a, b| a.name.cmp(&b.name));
    flags.iter().map(|flag| flag.allowed_spelling()).collect()
}
