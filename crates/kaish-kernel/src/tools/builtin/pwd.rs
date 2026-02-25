//! pwd — Print working directory.

use async_trait::async_trait;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema};

/// Pwd tool: print current working directory.
pub struct Pwd;

#[async_trait]
impl Tool for Pwd {
    fn name(&self) -> &str {
        "pwd"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("pwd", "Print current working directory")
            .example("Show current directory", "pwd")
    }

    async fn execute(&self, _args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        ExecResult::with_output(OutputData::text(ctx.cwd.to_string_lossy().to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::path::PathBuf;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_pwd_default() {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = Pwd.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "/");
    }

    #[tokio::test]
    async fn test_pwd_changed() {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.set_cwd(PathBuf::from("/mnt/project"));

        let result = Pwd.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "/mnt/project");
    }
}
