//! MCP tool wrapper that implements the kaish Tool trait.

use std::sync::Arc;

use async_trait::async_trait;
use opentelemetry::trace::TraceContextExt;
use rmcp::model::{RawContent, ResourceContents, Tool as McpTool};
use tracing_opentelemetry::OpenTelemetrySpanExt;

use kaish_kernel::ast::Value;
use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

use crate::client::McpClient;

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

    /// Get the parameter names in a consistent order.
    ///
    /// The MCP schema properties are in a Map which doesn't guarantee order.
    /// We try to use the "required" array first (which is ordered), then
    /// append any remaining optional parameters.
    fn param_names(&self) -> Vec<String> {
        let input_schema = &self.mcp_tool.input_schema;

        // Get required params first (they have a defined order in the array)
        let required_params: Vec<String> = input_schema
            .get("required")
            .and_then(|r| r.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default();

        // Get all properties
        let all_props: Vec<String> = input_schema
            .get("properties")
            .and_then(|p| p.as_object())
            .map(|obj| obj.keys().cloned().collect())
            .unwrap_or_default();

        // Start with required params, then add optional ones
        let mut result = required_params.clone();
        for prop in all_props {
            if !required_params.contains(&prop) {
                result.push(prop);
            }
        }

        result
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

        // Get parameter names in a consistent order for positional arg mapping
        let param_names = self.param_names();

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

            // Use param_names order to ensure consistent ordering
            for param_name in &param_names {
                if let Some(param_schema) = properties.get(param_name) {
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
        }

        schema
    }

    #[tracing::instrument(level = "info", skip(args, _ctx), fields(mcp_server = %self.client.name(), tool = %self.mcp_tool.name))]
    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        // Get parameter names for positional arg mapping
        let param_names = self.param_names();

        // Convert kaish args to JSON arguments
        let arguments = args_to_json(&args, &param_names);

        // Build trace context meta for outbound propagation
        let cx = tracing::Span::current().context();
        let span_ref = cx.span();
        let sc = span_ref.span_context();
        let meta = if sc.is_valid() {
            let mut m = serde_json::Map::new();
            m.insert(
                "traceparent".into(),
                format!(
                    "00-{}-{}-{:02x}",
                    sc.trace_id(),
                    sc.span_id(),
                    sc.trace_flags().to_u8()
                )
                .into(),
            );
            Some(m)
        } else {
            None
        };

        // Call the MCP tool
        match self.client.call_tool(&self.mcp_tool.name, arguments, meta).await {
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
///
/// The `param_names` are used to map positional arguments to their
/// corresponding parameter names in order.
fn args_to_json(
    args: &ToolArgs,
    param_names: &[String],
) -> Option<serde_json::Map<String, serde_json::Value>> {
    let mut map = serde_json::Map::new();

    // Add positional arguments mapped to parameter names in order
    for (i, value) in args.positional.iter().enumerate() {
        if let Some(param_name) = param_names.get(i) {
            map.insert(param_name.clone(), value_to_json(value));
        }
    }

    // Add named arguments (these override positional if same name)
    for (key, value) in &args.named {
        map.insert(key.clone(), value_to_json(value));
    }

    // Add flags as boolean true
    for flag in &args.flags {
        map.insert(flag.clone(), serde_json::Value::Bool(true));
    }

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
        Value::Json(json) => json.clone(),
        Value::Blob(blob) => {
            let mut map = serde_json::Map::new();
            map.insert("_type".to_string(), serde_json::Value::String("blob".to_string()));
            map.insert("id".to_string(), serde_json::Value::String(blob.id.clone()));
            map.insert("size".to_string(), serde_json::Value::Number(blob.size.into()));
            map.insert("contentType".to_string(), serde_json::Value::String(blob.content_type.clone()));
            if let Some(hash) = &blob.hash {
                let hash_hex: String = hash.iter().map(|b| format!("{:02x}", b)).collect();
                map.insert("hash".to_string(), serde_json::Value::String(hash_hex));
            }
            serde_json::Value::Object(map)
        }
    }
}
