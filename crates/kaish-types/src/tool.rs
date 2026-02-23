//! Tool schema and argument types.

use std::collections::{HashMap, HashSet};

use crate::value::Value;

/// Schema for a tool parameter.
#[derive(Debug, Clone)]
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
        }
    }

    /// Add alternative names/flags for this parameter.
    ///
    /// Aliases are used for short flags like `-r`, `-R` that map to `recursive`.
    pub fn with_aliases(mut self, aliases: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.aliases = aliases.into_iter().map(Into::into).collect();
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
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone)]
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
        }
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
}

/// Parsed arguments ready for tool execution.
#[derive(Debug, Clone, Default)]
pub struct ToolArgs {
    /// Positional arguments in order.
    pub positional: Vec<Value>,
    /// Named arguments by key.
    pub named: HashMap<String, Value>,
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
}
