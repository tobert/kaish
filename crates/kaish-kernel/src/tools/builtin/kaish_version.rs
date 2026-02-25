//! kaish-version — Print kaish version.

use async_trait::async_trait;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema};

/// kaish-version: prints the kaish version string.
pub struct KaishVersion;

#[async_trait]
impl Tool for KaishVersion {
    fn name(&self) -> &str {
        "kaish-version"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("kaish-version", "Print kaish version")
            .example("Show version", "kaish-version")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        let version = env!("CARGO_PKG_VERSION");
        ExecResult::with_output(OutputData::text(format!("kaish {version}\n")))
    }
}
