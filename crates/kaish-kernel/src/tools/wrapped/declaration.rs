//! The declaration: what an embedder writes, and what `build()` checks.
//!
//! A declaration is data. Every type here derives `serde::{Serialize,
//! Deserialize}` and holds no closures, so an embedder that keeps its policy
//! in a TOML or JSON file gets one with `toml::from_str`. The kernel owns no
//! config-file parser.

use std::collections::BTreeMap;
use std::path::PathBuf;

use anyhow::{bail, Result};
use serde::{Deserialize, Serialize};

use super::WrappedTool;

/// What happens to argv the declaration does not describe.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Tail {
    /// Refuse: `unexpected argument 'X'`. The default.
    #[default]
    Deny,
    /// Forward the tail after a rendered `--`.
    AfterDashDash,
    /// Forward the tail after a rendered `--`, and forward undeclared flags
    /// in place among the positionals.
    Forward,
}

/// What the child's standard input is connected to.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Stdin {
    /// `/dev/null`. The default.
    #[default]
    Closed,
    /// The kernel's stdin, streamed to the child.
    Pipe,
}

/// How a value flag joins its value in the rendered argv.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Style {
    /// `--name value` — two argv words.
    Separate,
    /// `--name=value` — one argv word.
    Equals,
}

/// A switch or a value flag, with its render style.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Flag {
    /// The declared name. Rendered `--name`, or `-n` for a one-character name.
    pub(crate) name: String,
    /// Alternative spellings, as written in the declaration (`-n`).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub(crate) aliases: Vec<String>,
    /// False for a switch, true for a flag that binds a value.
    #[serde(default)]
    pub(crate) takes_value: bool,
    /// May appear more than once; each occurrence renders.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub(crate) repeatable: bool,
    /// Must be present.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub(crate) required: bool,
    /// The value must parse as an `i64`.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub(crate) int: bool,
    /// The value must be in this set.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub(crate) choices: Vec<String>,
    /// Override the default render style for a program that accepts one form.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) style: Option<Style>,
    /// Published to agents through the tool schema.
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub(crate) about: String,
}

impl Flag {
    /// A flag that binds no value: `--oneline`.
    pub fn switch(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            aliases: Vec::new(),
            takes_value: false,
            repeatable: false,
            required: false,
            int: false,
            choices: Vec::new(),
            style: None,
            about: String::new(),
        }
    }

    /// A flag that binds the next word, or the text after `=`: `--since 2d`.
    pub fn value(name: impl Into<String>) -> Self {
        Self {
            takes_value: true,
            ..Self::switch(name)
        }
    }

    /// Add an alternative spelling, as the agent writes it (`-n`).
    pub fn alias(mut self, alias: impl Into<String>) -> Self {
        self.aliases.push(alias.into());
        self
    }

    /// Allow more than one occurrence; each one renders, in source order.
    pub fn repeatable(mut self) -> Self {
        self.repeatable = true;
        self
    }

    /// Require the flag.
    pub fn required(mut self) -> Self {
        self.required = true;
        self
    }

    /// Require the value to parse as an `i64`.
    pub fn int(mut self) -> Self {
        self.int = true;
        self
    }

    /// Require the value to be in this set.
    pub fn choices(mut self, choices: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.choices = choices.into_iter().map(Into::into).collect();
        self
    }

    /// Render under this style instead of the default for the name's length.
    pub fn style(mut self, style: Style) -> Self {
        self.style = Some(style);
        self
    }

    /// Describe the flag for agents. Published through the tool schema.
    pub fn about(mut self, about: impl Into<String>) -> Self {
        self.about = about.into();
        self
    }

    /// The name as the child sees it: `--max-count`, or `-m` for a
    /// one-character name.
    pub(crate) fn written_name(&self) -> String {
        written_form(&self.name)
    }

    /// The style this flag renders under: its override, or `Equals` for a
    /// long name and `Separate` for a one-character one.
    pub(crate) fn effective_style(&self) -> Style {
        self.style.unwrap_or({
            if self.name.chars().count() == 1 {
                Style::Separate
            } else {
                Style::Equals
            }
        })
    }

    /// Every spelling that selects this flag: its aliases as written, then
    /// its own written name.
    pub(crate) fn spellings(&self) -> Vec<String> {
        let mut spellings: Vec<String> = self.aliases.iter().map(|a| written_form(a)).collect();
        spellings.push(self.written_name());
        spellings
    }

    /// True when `spelling` selects this flag: one of its aliases as
    /// written, or its own written name.
    pub(crate) fn matches(&self, spelling: &str) -> bool {
        self.written_name() == spelling
            || self.aliases.iter().any(|alias| written_form(alias) == spelling)
    }

    /// How the allowed set names this flag: `-n/--max-count`, or `--oneline`
    /// when it has no alias.
    pub(crate) fn allowed_spelling(&self) -> String {
        self.spellings().join("/")
    }
}

/// One or many positional arguments, with optional constraints.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Positional {
    /// The declared name. Names the slot in errors and in the schema.
    pub(crate) name: String,
    /// Absorbs every remaining positional. Only the last slot may.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub(crate) many: bool,
    /// Must be present. On a `many` slot this means at least one.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub(crate) required: bool,
    /// The resolved real path must be inside this directory.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) path_under: Option<PathBuf>,
    /// Published to agents through the tool schema.
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub(crate) about: String,
}

impl Positional {
    /// A slot that takes exactly one word.
    pub fn one(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            many: false,
            required: false,
            path_under: None,
            about: String::new(),
        }
    }

    /// A slot that absorbs every remaining word. Must be the last slot.
    pub fn many(name: impl Into<String>) -> Self {
        Self {
            many: true,
            ..Self::one(name)
        }
    }

    /// Require the slot to be filled.
    pub fn required(mut self) -> Self {
        self.required = true;
        self
    }

    /// Require the resolved real path to be inside `root`.
    pub fn path_under(mut self, root: impl Into<PathBuf>) -> Self {
        self.path_under = Some(root.into());
        self
    }

    /// Describe the slot for agents. Published through the tool schema.
    pub fn about(mut self, about: impl Into<String>) -> Self {
        self.about = about.into();
        self
    }
}

/// A subcommand, or the root for a verb-less program like `python`.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Verb {
    /// `None` for the root verb.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) name: Option<String>,
    /// Argv words the kernel inserts after the verb name.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub(crate) lead: Vec<String>,
    /// Do not render the verb name; `lead` supplies the real argv. Used for a
    /// verb the program does not have (`python json-tool` → `-m json.tool`).
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub(crate) omit_name: bool,
    /// Declared flags, in declaration order — which is also render order.
    #[serde(default, alias = "flag", skip_serializing_if = "Vec::is_empty")]
    pub(crate) flags: Vec<Flag>,
    /// Declared positional slots, in slot order.
    #[serde(default, alias = "positional", skip_serializing_if = "Vec::is_empty")]
    pub(crate) positionals: Vec<Positional>,
    /// What happens to argv this verb does not describe.
    #[serde(default)]
    pub(crate) tail: Tail,
    /// What the child's standard input is connected to.
    #[serde(default)]
    pub(crate) stdin: Stdin,
    /// The verb's stdout is JSON, so `$(…)` binds it typed.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub(crate) json_output: bool,
    /// Published to agents through the tool schema.
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub(crate) about: String,
    /// Usage examples, published through the tool schema.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub(crate) examples: Vec<(String, String)>,
}

impl Verb {
    /// A named subcommand: `git log`.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: Some(name.into()),
            ..Self::default()
        }
    }

    /// The root verb, for a program whose first word is not a subcommand.
    pub fn root() -> Self {
        Self::default()
    }

    /// Argv words the kernel inserts after the verb name.
    pub fn lead(mut self, lead: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.lead = lead.into_iter().map(Into::into).collect();
        self
    }

    /// Select this verb by name but do not render the name; `lead` carries
    /// the real argv. `Verb::new("json-tool").lead(["-m", "json.tool"])
    /// .omit_name()` renders `python -m json.tool`.
    pub fn omit_name(mut self) -> Self {
        self.omit_name = true;
        self
    }

    /// Declare a flag. Declaration order is render order.
    pub fn flag(mut self, flag: Flag) -> Self {
        self.flags.push(flag);
        self
    }

    /// Declare a positional slot. Declaration order is slot order.
    pub fn positional(mut self, positional: Positional) -> Self {
        self.positionals.push(positional);
        self
    }

    /// Set what happens to argv this verb does not describe.
    pub fn tail(mut self, tail: Tail) -> Self {
        self.tail = tail;
        self
    }

    /// Set what the child's standard input is connected to.
    pub fn stdin(mut self, stdin: Stdin) -> Self {
        self.stdin = stdin;
        self
    }

    /// Declare that this verb's stdout is JSON, so `$(…)` binds it typed.
    pub fn json_output(mut self) -> Self {
        self.json_output = true;
        self
    }

    /// Describe the verb for agents. Published through the tool schema.
    pub fn about(mut self, about: impl Into<String>) -> Self {
        self.about = about.into();
        self
    }

    /// Add a usage example, published through the tool schema.
    pub fn example(mut self, label: impl Into<String>, command: impl Into<String>) -> Self {
        self.examples.push((label.into(), command.into()));
        self
    }

    /// The verb's name, or the empty string for the root.
    pub(crate) fn name_or_root(&self) -> &str {
        self.name.as_deref().unwrap_or("")
    }
}

/// One executable: pinned path, fixed lead argv, env pins, verbs.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct WrappedCommand {
    /// The tool name agents write: `git`.
    pub(crate) name: String,
    /// The pinned executable. Absolute, verified at `build()`.
    #[serde(default)]
    pub(crate) executable: PathBuf,
    /// Published to agents through the tool schema.
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub(crate) about: String,
    /// Argv words the kernel inserts before the verb name.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub(crate) lead: Vec<String>,
    /// Environment pins for the child. They win over exported scope variables.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub(crate) env: BTreeMap<String, String>,
    /// The verb selected when the first word names no declared verb.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) root: Option<Verb>,
    /// Named verbs, in declaration order.
    #[serde(default, alias = "verb", skip_serializing_if = "Vec::is_empty")]
    pub(crate) verbs: Vec<Verb>,
    /// Usage examples, published through the tool schema.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub(crate) examples: Vec<(String, String)>,
}

impl WrappedCommand {
    /// Start a declaration for the tool name agents will write.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Self::default()
        }
    }

    /// Pin the executable. `build()` verifies it.
    pub fn executable(mut self, executable: impl Into<PathBuf>) -> Self {
        self.executable = executable.into();
        self
    }

    /// Describe the command for agents. Published through the tool schema.
    pub fn about(mut self, about: impl Into<String>) -> Self {
        self.about = about.into();
        self
    }

    /// Argv words the kernel inserts before the verb name.
    pub fn lead(mut self, lead: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.lead = lead.into_iter().map(Into::into).collect();
        self
    }

    /// Pin an environment variable for the child. Pins win over exported
    /// scope variables of the same name.
    pub fn env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env.insert(key.into(), value.into());
        self
    }

    /// Declare the root verb, for a program whose first word is not a
    /// subcommand. A declaration may carry a root and named verbs at once:
    /// the first word selects a named verb when it matches one exactly, and
    /// otherwise falls through to the root.
    pub fn root(mut self, root: Verb) -> Self {
        self.root = Some(root);
        self
    }

    /// Declare a named verb.
    pub fn verb(mut self, verb: Verb) -> Self {
        self.verbs.push(verb);
        self
    }

    /// Add a usage example, published through the tool schema.
    pub fn example(mut self, label: impl Into<String>, command: impl Into<String>) -> Self {
        self.examples.push((label.into(), command.into()));
        self
    }

    /// Check the declaration and verify the executable, producing the tool.
    ///
    /// Fails when the declaration cannot describe a call unambiguously
    /// (duplicate names, a `many` slot that is not last, a required slot
    /// after an optional one), and when the executable is not an absolute
    /// path to an existing, executable file. A registration-time `Err` beats a
    /// `127` on first call.
    pub fn build(self) -> Result<WrappedTool> {
        self.check_shape()?;
        let executable = self.check_executable()?;
        Ok(WrappedTool::from_parts(self, executable))
    }

    fn check_shape(&self) -> Result<()> {
        if self.name.is_empty() {
            bail!("a wrapped command needs a name");
        }
        if self.root.is_none() && self.verbs.is_empty() {
            bail!(
                "wrapped command '{}' declares no verbs and no root, so it can accept no call",
                self.name
            );
        }
        if let Some(root) = &self.root
            && root.name.is_some()
        {
            bail!(
                "wrapped command '{}' passed a named verb to root(); use Verb::root()",
                self.name
            );
        }

        let mut seen: Vec<&str> = Vec::new();
        for verb in &self.verbs {
            let Some(name) = verb.name.as_deref() else {
                bail!(
                    "wrapped command '{}' passed an unnamed verb to verb(); use root()",
                    self.name
                );
            };
            if name.is_empty() {
                bail!("wrapped command '{}' declares a verb with no name", self.name);
            }
            if seen.contains(&name) {
                bail!(
                    "wrapped command '{}' declares the verb '{name}' twice",
                    self.name
                );
            }
            seen.push(name);
        }

        for verb in self.root.iter().chain(self.verbs.iter()) {
            self.check_verb(verb)?;
        }
        Ok(())
    }

    fn check_verb(&self, verb: &Verb) -> Result<()> {
        let scope = self.scope_of(verb);

        let mut spellings: Vec<String> = Vec::new();
        for flag in &verb.flags {
            if flag.name.is_empty() {
                bail!("'{scope}' declares a flag with no name");
            }
            if !flag.takes_value && !flag.choices.is_empty() {
                bail!(
                    "'{scope}' declares choices on the switch '{}'; a switch binds no value",
                    flag.written_name()
                );
            }
            if !flag.takes_value && flag.int {
                bail!(
                    "'{scope}' declares int on the switch '{}'; a switch binds no value",
                    flag.written_name()
                );
            }
            for spelling in flag.spellings() {
                if spellings.contains(&spelling) {
                    bail!("'{scope}' declares the flag spelling '{spelling}' twice");
                }
                spellings.push(spelling);
            }
        }

        let mut seen_optional: Option<&str> = None;
        for (slot, positional) in verb.positionals.iter().enumerate() {
            if positional.name.is_empty() {
                bail!("'{scope}' declares a positional with no name");
            }
            if verb.positionals[..slot]
                .iter()
                .any(|p| p.name == positional.name)
            {
                bail!(
                    "'{scope}' declares the positional '{}' twice",
                    positional.name
                );
            }
            if positional.many && slot + 1 != verb.positionals.len() {
                bail!(
                    "'{scope}' declares the many positional '{}' before '{}'; a many \
                     positional absorbs the rest, so it must be last",
                    positional.name,
                    verb.positionals[slot + 1].name
                );
            }
            match (positional.required, seen_optional) {
                (true, Some(optional)) => bail!(
                    "'{scope}' declares the required positional '{}' after the optional \
                     '{optional}'; no call could fill '{optional}' without it",
                    positional.name
                ),
                (false, None) => seen_optional = Some(&positional.name),
                _ => {}
            }
            if let Some(root) = &positional.path_under
                && !root.is_absolute()
            {
                bail!(
                    "'{scope}' declares path_under({}) on '{}'; the root must be an \
                     absolute path",
                    root.display(),
                    positional.name
                );
            }
        }
        Ok(())
    }

    fn check_executable(&self) -> Result<PathBuf> {
        let path = &self.executable;
        if path.as_os_str().is_empty() {
            bail!("wrapped command '{}' declares no executable", self.name);
        }
        if !path.is_absolute() {
            bail!(
                "wrapped command '{}': executable {} is not an absolute path",
                self.name,
                path.display()
            );
        }
        let metadata = std::fs::metadata(path).map_err(|e| {
            anyhow::anyhow!(
                "wrapped command '{}': executable {} cannot be read ({e})",
                self.name,
                path.display()
            )
        })?;
        if !metadata.is_file() {
            bail!(
                "wrapped command '{}': executable {} is not a file",
                self.name,
                path.display()
            );
        }
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            if metadata.permissions().mode() & 0o111 == 0 {
                bail!(
                    "wrapped command '{}': executable {} has no execute bit",
                    self.name,
                    path.display()
                );
            }
        }
        Ok(path.clone())
    }

    /// `git log` for a named verb, `python` for the root.
    pub(crate) fn scope_of(&self, verb: &Verb) -> String {
        match &verb.name {
            Some(name) => format!("{} {name}", self.name),
            None => self.name.clone(),
        }
    }
}

/// The name as the child sees it: an explicitly dashed spelling verbatim,
/// `-x` for a one-character name, `--name` for anything longer.
pub(crate) fn written_form(name: &str) -> String {
    if name.starts_with('-') {
        name.to_string()
    } else if name.chars().count() == 1 {
        format!("-{name}")
    } else {
        format!("--{name}")
    }
}

/// Resolve a bare command name against a `PATH` string the embedder supplies.
///
/// The kernel does not read the OS environment to find one — the whole point
/// of a pinned executable is that the deployment names it.
pub fn find_executable(name: &str, path_var: &str) -> Option<PathBuf> {
    crate::tools::resolve_in_path(name, path_var).map(PathBuf::from)
}
