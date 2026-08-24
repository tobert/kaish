//! Tool registry for looking up and managing tools.

use std::collections::HashMap;
use std::sync::Arc;

use super::traits::{Tool, ToolSchema};

/// Registry of available tools.
#[derive(Default)]
pub struct ToolRegistry {
    tools: HashMap<String, Arc<dyn Tool>>,
}

impl ToolRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a tool.
    pub fn register(&mut self, tool: impl Tool + 'static) {
        let name = tool.name().to_string();
        self.tools.insert(name, Arc::new(tool));
    }

    /// Register a tool that's already in an Arc.
    pub fn register_arc(&mut self, tool: Arc<dyn Tool>) {
        let name = tool.name().to_string();
        self.tools.insert(name, tool);
    }

    /// Look up a tool by name.
    pub fn get(&self, name: &str) -> Option<Arc<dyn Tool>> {
        self.tools.get(name).cloned()
    }

    /// Check if a tool exists.
    pub fn contains(&self, name: &str) -> bool {
        self.tools.contains_key(name)
    }

    /// List all tool names.
    pub fn names(&self) -> Vec<&str> {
        let mut names: Vec<_> = self.tools.keys().map(|s| s.as_str()).collect();
        names.sort();
        names
    }

    /// List all tool schemas.
    pub fn schemas(&self) -> Vec<ToolSchema> {
        let mut schemas: Vec<_> = self.tools.values().map(|t| t.schema()).collect();
        schemas.sort_by(|a, b| a.name.cmp(&b.name));
        schemas
    }

    /// Number of registered tools.
    pub fn len(&self) -> usize {
        self.tools.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.tools.is_empty()
    }
}

impl std::fmt::Debug for ToolRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ToolRegistry")
            .field("tools", &self.names())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::ExecResult;
    use crate::tools::{ToolArgs, ToolCtx};
    use async_trait::async_trait;

    struct DummyTool;

    #[async_trait]
    impl Tool for DummyTool {
        fn name(&self) -> &str {
            "dummy"
        }

        fn schema(&self) -> ToolSchema {
            ToolSchema::new("dummy", "A test tool")
        }

        async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
            ExecResult::success("dummy output")
        }
    }

    #[test]
    fn test_register_and_get() {
        let mut registry = ToolRegistry::new();
        registry.register(DummyTool);

        assert!(registry.contains("dummy"));
        assert!(registry.get("dummy").is_some());
        assert!(!registry.contains("nonexistent"));
    }

    /// `validate_command` (validator/walker.rs) falls back to `tool.schema()`
    /// on a catalog miss, and `validate_against_schema` attributes every
    /// `ValidationIssue` it raises to that schema's `.name` — not to the AST
    /// `cmd.name` the walker actually resolved and dispatched by (`==
    /// tool.name()`, since `register`/`register_arc` key this map on it).
    /// Nothing at the `Tool` trait level keeps `name()` and `schema().name`
    /// in sync; a tool whose two names disagree would silently misattribute
    /// every issue it raises to the wrong command.
    ///
    /// This test is the actual enforcement of that invariant. A
    /// `debug_assert!` at the call site would compile to nothing in a
    /// release build — this workspace has no `[profile.release]` override,
    /// so `debug_assertions` is off wherever kaish actually ships — while
    /// this test runs in CI on every `cargo test` and covers every in-tree
    /// builtin at once, not only the ones a particular script happens to
    /// exercise.
    ///
    /// Only covers what `register_builtins` installs — a third-party tool
    /// an embedder registers at runtime is not walked here and is not
    /// covered by anything else either.
    #[test]
    fn every_builtin_tool_name_matches_its_own_schema_name() {
        let mut registry = ToolRegistry::new();
        crate::tools::register_builtins(&mut registry);

        for name in registry.names() {
            let tool = registry.get(name).expect("a name from names() must resolve via get()");
            assert_eq!(
                tool.name(),
                tool.schema().name.as_str(),
                "tool '{name}': Tool::name() == {:?} but Tool::schema().name == {:?} -- \
                 these must agree. validate_against_schema attributes every ValidationIssue \
                 it raises to schema.name, while the walker resolves and dispatches the \
                 command by cmd.name (== tool.name()); a mismatch here means an embedder \
                 reading ValidationIssue::command sees the wrong command. Fix the tool so \
                 Tool::name() and Tool::schema().name agree -- do not relax this test.",
                tool.name(),
                tool.schema().name,
            );
        }
    }

    #[test]
    fn test_names_sorted() {
        let mut registry = ToolRegistry::new();

        struct ToolA;
        struct ToolZ;

        #[async_trait]
        impl Tool for ToolA {
            fn name(&self) -> &str { "aaa" }
            fn schema(&self) -> ToolSchema { ToolSchema::new("aaa", "") }
            async fn execute(&self, _: ToolArgs, _: &mut dyn ToolCtx) -> ExecResult {
                ExecResult::success("")
            }
        }

        #[async_trait]
        impl Tool for ToolZ {
            fn name(&self) -> &str { "zzz" }
            fn schema(&self) -> ToolSchema { ToolSchema::new("zzz", "") }
            async fn execute(&self, _: ToolArgs, _: &mut dyn ToolCtx) -> ExecResult {
                ExecResult::success("")
            }
        }

        registry.register(ToolZ);
        registry.register(ToolA);

        let names = registry.names();
        assert_eq!(names, vec!["aaa", "zzz"]);
    }
}
