//! hostname — Print the system hostname.
//!
//! # Examples
//!
//! ```kaish
//! hostname                       # → myhost
//! ```

use async_trait::async_trait;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema};

use super::uname::read_hostname;

/// Hostname tool: print the system hostname.
pub struct Hostname;

#[async_trait]
impl Tool for Hostname {
    fn name(&self) -> &str {
        "hostname"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("hostname", "Print the system hostname")
            .example("Print hostname", "hostname")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        ExecResult::with_output(OutputData::text(read_hostname()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    #[tokio::test]
    async fn test_hostname_returns_nonempty() {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = Hostname.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(!result.out.is_empty(), "hostname should return a non-empty string");
        // Should not contain trailing newline
        assert!(!result.out.contains('\n'));
    }
}
