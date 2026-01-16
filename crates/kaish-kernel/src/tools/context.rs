//! Execution context for tools.

use std::path::PathBuf;
use std::sync::Arc;

use crate::interpreter::Scope;
use crate::vfs::VfsRouter;

use super::traits::ToolSchema;

/// Execution context passed to tools.
///
/// Provides access to the VFS, scope, and other kernel state.
pub struct ExecContext {
    /// Virtual filesystem.
    pub vfs: Arc<VfsRouter>,
    /// Variable scope.
    pub scope: Scope,
    /// Current working directory (VFS path).
    pub cwd: PathBuf,
    /// Previous working directory (for `cd -`).
    pub prev_cwd: Option<PathBuf>,
    /// Standard input for the tool (from pipeline).
    pub stdin: Option<String>,
    /// Tool schemas for help command.
    pub tool_schemas: Vec<ToolSchema>,
}

impl ExecContext {
    /// Create a new execution context.
    pub fn new(vfs: Arc<VfsRouter>) -> Self {
        Self {
            vfs,
            scope: Scope::new(),
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            tool_schemas: Vec::new(),
        }
    }

    /// Create a context with a specific scope.
    pub fn with_scope(vfs: Arc<VfsRouter>, scope: Scope) -> Self {
        Self {
            vfs,
            scope,
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            tool_schemas: Vec::new(),
        }
    }

    /// Set the available tool schemas (for help command).
    pub fn set_tool_schemas(&mut self, schemas: Vec<ToolSchema>) {
        self.tool_schemas = schemas;
    }

    /// Set stdin for this execution.
    pub fn set_stdin(&mut self, stdin: String) {
        self.stdin = Some(stdin);
    }

    /// Get stdin, consuming it.
    pub fn take_stdin(&mut self) -> Option<String> {
        self.stdin.take()
    }

    /// Resolve a path relative to cwd.
    pub fn resolve_path(&self, path: &str) -> PathBuf {
        if path.starts_with('/') {
            PathBuf::from(path)
        } else {
            self.cwd.join(path)
        }
    }

    /// Change the current working directory.
    ///
    /// Saves the old directory for `cd -` support.
    pub fn set_cwd(&mut self, path: PathBuf) {
        self.prev_cwd = Some(self.cwd.clone());
        self.cwd = path;
    }

    /// Get the previous working directory (for `cd -`).
    pub fn get_prev_cwd(&self) -> Option<&PathBuf> {
        self.prev_cwd.as_ref()
    }
}
