//! Core tool traits and types.

use async_trait::async_trait;
use std::collections::{HashMap, HashSet};

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputFormat};
use crate::validator::{IssueCode, Severity, ValidationIssue};

use super::context::ExecContext;

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
}

impl ToolSchema {
    /// Create a new tool schema.
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            params: Vec::new(),
            examples: Vec::new(),
        }
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

/// A tool that can be executed.
#[async_trait]
pub trait Tool: Send + Sync {
    /// The tool's name (used for lookup).
    fn name(&self) -> &str;

    /// Get the tool's schema.
    fn schema(&self) -> ToolSchema;

    /// Execute the tool with the given arguments and context.
    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult;

    /// Validate arguments without executing.
    ///
    /// Default implementation validates against the schema.
    /// Override this for semantic checks (regex validity, zero increment, etc.).
    fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        validate_against_schema(args, &self.schema())
    }
}

/// Validate arguments against a tool schema.
///
/// Checks:
/// - Required parameters are provided
/// - Unknown flags (warnings)
/// - Type compatibility
pub fn validate_against_schema(args: &ToolArgs, schema: &ToolSchema) -> Vec<ValidationIssue> {
    let mut issues = Vec::new();

    // Check required parameters
    for (i, param) in schema.params.iter().enumerate() {
        if !param.required {
            continue;
        }

        // Check named args first, then positional
        let has_named = args.named.contains_key(&param.name);
        let has_positional = args.positional.len() > i;
        let has_flag = param.param_type == "bool" && args.has_flag(&param.name);

        if !has_named && !has_positional && !has_flag {
            let code = IssueCode::MissingRequiredArg;
            issues.push(ValidationIssue {
                severity: code.default_severity(),
                code,
                message: format!("required parameter '{}' not provided", param.name),
                span: None,
                suggestion: Some(format!("add {} or {}=<value>", param.name, param.name)),
            });
        }
    }

    // Check for unknown flags (only warn - tools may accept dynamic flags)
    let known_flags: HashSet<&str> = schema
        .params
        .iter()
        .filter(|p| p.param_type == "bool")
        .flat_map(|p| {
            std::iter::once(p.name.as_str())
                .chain(p.aliases.iter().map(|a| a.as_str()))
        })
        .collect();

    for flag in &args.flags {
        // Strip leading dashes for comparison
        let flag_name = flag.trim_start_matches('-');
        // Global output flags are handled by the kernel, not the tool
        if is_global_output_flag(flag_name) {
            continue;
        }
        if !known_flags.contains(flag_name) && !known_flags.contains(flag.as_str()) {
            // Check if any param matches this flag via aliases
            let matches_alias = schema.params.iter().any(|p| p.matches_flag(flag));
            if !matches_alias {
                issues.push(ValidationIssue {
                    severity: Severity::Warning,
                    code: IssueCode::UnknownFlag,
                    message: format!("unknown flag '{}'", flag),
                    span: None,
                    suggestion: None,
                });
            }
        }
    }

    // Check type compatibility for named args
    for (key, value) in &args.named {
        if let Some(param) = schema.params.iter().find(|p| &p.name == key)
            && let Some(issue) = check_type_compatibility(key, value, &param.param_type) {
                issues.push(issue);
            }
    }

    // Check type compatibility for positional args
    for (i, value) in args.positional.iter().enumerate() {
        if let Some(param) = schema.params.get(i)
            && let Some(issue) = check_type_compatibility(&param.name, value, &param.param_type) {
                issues.push(issue);
            }
    }

    issues
}

// ============================================================
// Global Output Flags (--json, --toon)
// ============================================================

/// Registry of global output format flags.
const GLOBAL_OUTPUT_FLAGS: &[(&str, OutputFormat)] = &[
    ("json", OutputFormat::Json),
    ("toon", OutputFormat::Toon),
];

/// Check if a flag name is a global output flag.
pub fn is_global_output_flag(name: &str) -> bool {
    GLOBAL_OUTPUT_FLAGS.iter().any(|(n, _)| *n == name)
}

/// Extract and remove a global output format flag from ToolArgs.
///
/// Only applies to known tools with a schema. External commands
/// (schema=None) must receive their flags untouched —
/// `cargo --json` must not have --json stripped by the kernel.
pub fn extract_output_format(
    args: &mut ToolArgs,
    schema: Option<&ToolSchema>,
) -> Option<OutputFormat> {
    // External commands keep their flags
    let _schema = schema?;

    for (flag_name, format) in GLOBAL_OUTPUT_FLAGS {
        if args.flags.remove(*flag_name) {
            return Some(*format);
        }
    }
    None
}

/// Check if a value is compatible with a type.
fn check_type_compatibility(name: &str, value: &Value, expected_type: &str) -> Option<ValidationIssue> {
    let compatible = match expected_type {
        "any" => true,
        "string" => true, // Everything can be a string
        "int" => matches!(value, Value::Int(_) | Value::String(_)),
        "float" => matches!(value, Value::Float(_) | Value::Int(_) | Value::String(_)),
        "bool" => matches!(value, Value::Bool(_) | Value::String(_)),
        "array" => matches!(value, Value::String(_)), // Arrays are passed as strings in kaish
        "object" => matches!(value, Value::String(_)), // Objects are JSON strings
        _ => true, // Unknown types pass
    };

    if compatible {
        None
    } else {
        let code = IssueCode::InvalidArgType;
        Some(ValidationIssue {
            severity: code.default_severity(),
            code,
            message: format!(
                "argument '{}' has type {:?}, expected {}",
                name, value, expected_type
            ),
            span: None,
            suggestion: None,
        })
    }
}
