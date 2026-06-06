//! MCP prompt implementations.
//!
//! Each kaish help topic becomes an MCP prompt. Uses the same content
//! as the `help` builtin via `kaish_kernel::help::get_help()`.
//!
//! The `#[prompt_router]` impl block lives in `handler.rs` alongside
//! `#[tool_router]` so both routers share the same module visibility.

use rmcp::schemars::{self, JsonSchema};
use serde::{Deserialize, Serialize};

/// Parameters for the builtins prompt (accepts optional tool name filter).
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct BuiltinsParams {
    /// Specific tool name to get detailed help for (optional).
    #[schemars(description = "Specific tool name for detailed help (optional)")]
    pub tool: Option<String>,
}

#[cfg(test)]
mod tests {
    use crate::server::config::McpServerConfig;
    use crate::server::handler::KaishServerHandler;
    use rmcp::handler::server::wrapper::Parameters;
    use rmcp::model::PromptMessageContent;

    use super::*;

    fn make_handler() -> KaishServerHandler {
        let config = McpServerConfig::default();
        KaishServerHandler::new(config, vec![]).expect("handler creation failed")
    }

    #[tokio::test]
    async fn test_prompt_overview() {
        let handler = make_handler();
        let result = handler
            .prompt_overview()
            .await
            .expect("prompt_overview failed");
        assert!(result.description.is_some());
        assert!(!result.messages.is_empty());
        if let PromptMessageContent::Text { ref text } = result.messages[0].content {
            assert!(text.contains("kaish"), "overview should mention kaish");
        } else {
            panic!("Expected text content");
        }
    }

    #[tokio::test]
    async fn test_prompt_syntax() {
        let handler = make_handler();
        let result = handler
            .prompt_syntax()
            .await
            .expect("prompt_syntax failed");
        assert!(!result.messages.is_empty());
        if let PromptMessageContent::Text { ref text } = result.messages[0].content {
            assert!(text.contains("Variables"), "syntax should mention Variables");
        } else {
            panic!("Expected text content");
        }
    }

    #[tokio::test]
    async fn test_prompt_builtins_all() {
        let handler = make_handler();
        let params = Parameters(BuiltinsParams { tool: None });
        let result = handler
            .prompt_builtins(params)
            .await
            .expect("prompt_builtins failed");
        assert!(!result.messages.is_empty());
        if let PromptMessageContent::Text { ref text } = result.messages[0].content {
            assert!(text.contains("echo"), "builtins should list echo");
        } else {
            panic!("Expected text content");
        }
    }

    #[tokio::test]
    async fn test_prompt_builtins_specific_tool() {
        let handler = make_handler();
        let params = Parameters(BuiltinsParams {
            tool: Some("echo".to_string()),
        });
        let result = handler
            .prompt_builtins(params)
            .await
            .expect("prompt_builtins failed");
        assert!(!result.messages.is_empty());
    }

    #[tokio::test]
    async fn test_prompt_vfs() {
        let handler = make_handler();
        let result = handler.prompt_vfs().await.expect("prompt_vfs failed");
        assert!(!result.messages.is_empty());
    }

    #[tokio::test]
    async fn test_prompt_scatter() {
        let handler = make_handler();
        let result = handler
            .prompt_scatter()
            .await
            .expect("prompt_scatter failed");
        assert!(!result.messages.is_empty());
    }

    #[tokio::test]
    async fn test_prompt_limits() {
        let handler = make_handler();
        let result = handler
            .prompt_limits()
            .await
            .expect("prompt_limits failed");
        assert!(!result.messages.is_empty());
    }

    #[tokio::test]
    async fn server_instructions_compose_core_and_keep_mcp_tail() {
        use rmcp::ServerHandler;
        let handler = make_handler();
        let info = handler.get_info();
        let instr = info.instructions.expect("instructions present");
        // The shared core comes from the kaish-help corpus (a Foundations guarantee).
        assert!(
            instr.contains("No word splitting"),
            "instructions should carry the composed core, got:\n{instr}"
        );
        // The MCP-specific tail stays in the handler (frontend, not language).
        assert!(instr.contains("kaish://vfs"), "should keep the MCP resource hint");
        assert!(instr.contains("--init"), "should keep the MCP --init hint");
    }

    #[test]
    fn test_prompt_router_lists_all() {
        let prompts = KaishServerHandler::prompt_router().list_all();
        assert_eq!(prompts.len(), 6, "should have 6 prompts");

        let names: Vec<&str> = prompts.iter().map(|p| p.name.as_str()).collect();
        assert!(names.contains(&"kaish-overview"));
        assert!(names.contains(&"kaish-syntax"));
        assert!(names.contains(&"kaish-builtins"));
        assert!(names.contains(&"kaish-vfs"));
        assert!(names.contains(&"kaish-scatter"));
        assert!(names.contains(&"kaish-limits"));
    }
}
