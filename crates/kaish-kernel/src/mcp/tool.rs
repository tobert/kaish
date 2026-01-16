//! MCP tool wrapper that implements the kaish Tool trait.

use std::sync::Arc;

use async_trait::async_trait;
use rmcp::model::{RawContent, ResourceContents, Tool as McpTool};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

use super::client::McpClient;

/// Wraps an MCP tool as a kaish Tool.
///
/// This allows MCP tools to be registered in the kaish ToolRegistry
/// and called like any other builtin tool.
pub struct McpToolWrapper {
    /// The MCP client to call tools on.
    client: Arc<McpClient>,
    /// The MCP tool definition.
    mcp_tool: McpTool,
    /// Prefixed name (e.g., "mcp:server:toolname").
    full_name: String,
}

impl McpToolWrapper {
    /// Create a new wrapper for an MCP tool.
    pub fn new(client: Arc<McpClient>, mcp_tool: McpTool) -> Self {
        let full_name = format!("{}:{}", client.name(), mcp_tool.name);
        Self {
            client,
            mcp_tool,
            full_name,
        }
    }

    /// Get the MCP tool definition.
    pub fn mcp_tool(&self) -> &McpTool {
        &self.mcp_tool
    }

    /// Get the client name prefix.
    pub fn client_name(&self) -> &str {
        self.client.name()
    }
}

#[async_trait]
impl Tool for McpToolWrapper {
    fn name(&self) -> &str {
        &self.full_name
    }

    fn schema(&self) -> ToolSchema {
        let mut schema = ToolSchema::new(
            &self.full_name,
            self.mcp_tool
                .description
                .as_deref()
                .unwrap_or("MCP tool"),
        );

        // Convert MCP input schema to kaish ParamSchema
        // input_schema is Arc<JsonObject> = Arc<Map<String, Value>>
        let input_schema = &self.mcp_tool.input_schema;

        if let Some(properties) = input_schema.get("properties").and_then(|p| p.as_object()) {
            let required_params: Vec<String> = input_schema
                .get("required")
                .and_then(|r| r.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect()
                })
                .unwrap_or_default();

            for (param_name, param_schema) in properties {
                let param_type = param_schema
                    .get("type")
                    .and_then(|t| t.as_str())
                    .unwrap_or("any")
                    .to_string();

                let description = param_schema
                    .get("description")
                    .and_then(|d| d.as_str())
                    .unwrap_or("")
                    .to_string();

                let is_required = required_params.contains(param_name);

                let param = if is_required {
                    ParamSchema::required(param_name, param_type, description)
                } else {
                    ParamSchema::optional(param_name, param_type, Value::Null, description)
                };

                schema = schema.param(param);
            }
        }

        schema
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        // Convert kaish args to JSON arguments
        let arguments = args_to_json(&args);

        // Call the MCP tool
        match self.client.call_tool(&self.mcp_tool.name, arguments).await {
            Ok(result) => {
                // Extract text content from the result
                let mut output = String::new();
                for content in &result.content {
                    // Annotated<RawContent> derefs to RawContent
                    match &content.raw {
                        RawContent::Text(text) => {
                            if !output.is_empty() {
                                output.push('\n');
                            }
                            output.push_str(&text.text);
                        }
                        RawContent::Image(img) => {
                            output.push_str(&format!("[image: {}]\n", img.mime_type));
                        }
                        RawContent::Audio(audio) => {
                            output.push_str(&format!("[audio: {}]\n", audio.mime_type));
                        }
                        RawContent::Resource(res) => {
                            let uri = match &res.resource {
                                ResourceContents::TextResourceContents { uri, .. } => uri,
                                ResourceContents::BlobResourceContents { uri, .. } => uri,
                            };
                            output.push_str(&format!("[resource: {}]\n", uri));
                        }
                        RawContent::ResourceLink(res) => {
                            output.push_str(&format!("[resource-link: {}]\n", res.uri));
                        }
                    }
                }

                if result.is_error.unwrap_or(false) {
                    ExecResult::failure(1, output)
                } else {
                    ExecResult::success(output)
                }
            }
            Err(e) => ExecResult::failure(1, format!("MCP tool error: {}", e)),
        }
    }
}

impl std::fmt::Debug for McpToolWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("McpToolWrapper")
            .field("name", &self.full_name)
            .field("client", &self.client.name())
            .finish()
    }
}

/// Convert kaish ToolArgs to JSON arguments for MCP.
fn args_to_json(args: &ToolArgs) -> Option<serde_json::Map<String, serde_json::Value>> {
    let mut map = serde_json::Map::new();

    // Add named arguments
    for (key, value) in &args.named {
        map.insert(key.clone(), value_to_json(value));
    }

    // Add flags as boolean true
    for flag in &args.flags {
        map.insert(flag.clone(), serde_json::Value::Bool(true));
    }

    // Positional args are harder to map without schema info
    // For now, we'll skip them as MCP tools typically use named params

    if map.is_empty() {
        None
    } else {
        Some(map)
    }
}

/// Convert a kaish Value to a JSON value.
fn value_to_json(value: &Value) -> serde_json::Value {
    match value {
        Value::Null => serde_json::Value::Null,
        Value::Bool(b) => serde_json::Value::Bool(*b),
        Value::Int(i) => serde_json::Value::Number((*i).into()),
        Value::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        Value::String(s) => serde_json::Value::String(s.clone()),
        Value::Array(items) => {
            // Array contains Expr, not Value - for now, just represent as strings
            serde_json::Value::Array(
                items
                    .iter()
                    .map(|_| serde_json::Value::String("[expr]".to_string()))
                    .collect(),
            )
        }
        Value::Object(fields) => {
            // Object contains (String, Expr) pairs - for now, represent keys
            let mut obj = serde_json::Map::new();
            for (key, _) in fields {
                obj.insert(key.clone(), serde_json::Value::String("[expr]".to_string()));
            }
            serde_json::Value::Object(obj)
        }
    }
}
