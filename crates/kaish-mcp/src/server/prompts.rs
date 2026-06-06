//! MCP prompt surface.
//!
//! The prompt set is single-sourced from the kaish-help topic registry
//! (`kaish_kernel::help::list_topics`): one `kaish-<topic>` prompt per help
//! topic, rendered through the same `get_help` content the `help` builtin uses.
//! The list/render logic lives on `KaishServerHandler` (`build_prompts` /
//! `render_prompt`) so it shares the handler's cached tool schemas; this module
//! holds the tests that pin the behavior.

#[cfg(test)]
mod tests {
    use crate::server::config::McpServerConfig;
    use crate::server::handler::KaishServerHandler;
    use rmcp::model::PromptMessageContent;
    use serde_json::{Map, Value};

    fn make_handler() -> KaishServerHandler {
        let config = McpServerConfig::default();
        KaishServerHandler::new(config, vec![]).expect("handler creation failed")
    }

    /// Extract the rendered text of a prompt's first message.
    fn first_text(result: &rmcp::model::GetPromptResult) -> &str {
        match &result.messages[0].content {
            PromptMessageContent::Text { text } => text.as_str(),
            _ => panic!("expected text content"),
        }
    }

    #[tokio::test]
    async fn prompt_list_is_sourced_from_topic_registry() {
        let handler = make_handler();
        let prompts = handler.build_prompts();

        // One prompt per help topic, all `kaish-`-prefixed.
        let topics = kaish_kernel::help::list_topics();
        assert_eq!(
            prompts.len(),
            topics.len(),
            "prompt count should track the topic registry"
        );
        for prompt in &prompts {
            assert!(
                prompt.name.starts_with("kaish-"),
                "prompt name should be kaish-prefixed: {}",
                prompt.name
            );
            assert!(prompt.description.is_some(), "every prompt has a description");
        }

        let names: Vec<&str> = prompts.iter().map(|p| p.name.as_str()).collect();
        // Spot-check the well-known topics carry over.
        for expected in [
            "kaish-overview",
            "kaish-syntax",
            "kaish-builtins",
            "kaish-vfs",
            "kaish-scatter",
            "kaish-limits",
        ] {
            assert!(names.contains(&expected), "missing prompt {expected}");
        }
    }

    #[tokio::test]
    async fn only_builtins_prompt_advertises_the_tool_argument() {
        let handler = make_handler();
        for prompt in handler.build_prompts() {
            match prompt.name.as_str() {
                "kaish-builtins" => {
                    let args = prompt
                        .arguments
                        .as_ref()
                        .expect("builtins prompt should advertise arguments");
                    assert_eq!(args.len(), 1);
                    assert_eq!(args[0].name, "tool");
                    assert_eq!(args[0].required, Some(false));
                }
                _ => assert!(
                    prompt.arguments.is_none(),
                    "{} should not advertise arguments",
                    prompt.name
                ),
            }
        }
    }

    #[tokio::test]
    async fn render_overview_mentions_kaish() {
        let handler = make_handler();
        let result = handler
            .render_prompt("kaish-overview", None)
            .expect("overview should render");
        assert!(result.description.is_some());
        assert!(first_text(&result).contains("kaish"));
    }

    #[tokio::test]
    async fn render_syntax_mentions_variables() {
        let handler = make_handler();
        let result = handler
            .render_prompt("kaish-syntax", None)
            .expect("syntax should render");
        assert!(first_text(&result).contains("Variables"));
    }

    #[tokio::test]
    async fn render_builtins_without_tool_lists_commands() {
        let handler = make_handler();
        let result = handler
            .render_prompt("kaish-builtins", None)
            .expect("builtins should render");
        assert!(first_text(&result).contains("echo"), "should list echo");
    }

    #[tokio::test]
    async fn render_builtins_with_tool_gives_tool_help() {
        let handler = make_handler();
        let mut args = Map::new();
        args.insert("tool".to_string(), Value::String("echo".to_string()));
        let result = handler
            .render_prompt("kaish-builtins", Some(&args))
            .expect("builtins(tool) should render");
        assert_eq!(
            result.description.as_deref(),
            Some("Help for builtin: echo")
        );
        assert!(!result.messages.is_empty());
    }

    #[tokio::test]
    async fn render_builtins_with_empty_tool_falls_back_to_index() {
        // An empty `tool` argument must not become Tool("") help.
        let handler = make_handler();
        let mut args = Map::new();
        args.insert("tool".to_string(), Value::String(String::new()));
        let result = handler
            .render_prompt("kaish-builtins", Some(&args))
            .expect("builtins(empty tool) should render the index");
        assert!(first_text(&result).contains("echo"));
    }

    #[tokio::test]
    async fn render_rejects_unknown_prompt() {
        let handler = make_handler();
        let err = handler
            .render_prompt("kaish-bogus", None)
            .expect_err("unknown prompt should fail loudly");
        assert!(
            err.message.contains("unknown prompt"),
            "error should name the failure: {}",
            err.message
        );
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
}
