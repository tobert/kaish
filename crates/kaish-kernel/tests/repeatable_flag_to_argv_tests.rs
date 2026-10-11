//! A repeatable value flag must reach a clap `Vec<String>` field intact.
//!
//! The kernel accumulates every occurrence of a repeatable flag (clap
//! `ArgAction::Append`) into one `Value::Json(Array)`. `ToolArgs::to_argv`
//! used to render that array as a single JSON token, so a tool that parsed
//! its own argv got `["a","b"]` as ONE path and matched nothing — exit 0,
//! empty output. Builtins read the raw `named` value to avoid this; an
//! external tool written the ordinary clap way had no such warning.
//!
//! These tests register a clap-derived tool the way an embedder does and
//! read the field back through `to_argv`.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use async_trait::async_trait;
use clap::Parser;
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend, Tool};
use kaish_types::{ExecResult, OutputData};
use rstest::rstest;

#[derive(Parser)]
#[command(name = "pathprobe", about = "Print the paths it was given")]
struct PathProbeArgs {
    /// A path. Repeatable.
    #[arg(short = 'p', long = "path")]
    path: Vec<String>,
    /// One value, not repeatable.
    #[arg(long = "one")]
    one: Option<String>,
}

struct PathProbe;

#[async_trait]
impl Tool for PathProbe {
    fn name(&self) -> &str {
        "pathprobe"
    }

    fn schema(&self) -> ToolSchema {
        kaish_tool_api::schema_from_clap(
            &<PathProbeArgs as clap::CommandFactory>::command(),
            "pathprobe",
            "Print the paths it was given",
            [],
        )
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        let argv = match args.to_argv() {
            Ok(a) => a,
            Err(e) => return ExecResult::failure(2, e.to_string()),
        };
        let parsed = match PathProbeArgs::try_parse_from(std::iter::once("pathprobe".to_string()).chain(argv)) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, e.to_string()),
        };
        let mut lines: Vec<String> = parsed.path.iter().map(|p| format!("path={p}")).collect();
        if let Some(one) = parsed.one {
            lines.push(format!("one={one}"));
        }
        ExecResult::with_output(OutputData::text(lines.join("\n")))
    }
}

fn kernel() -> Kernel {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |tools| {
        tools.register(PathProbe);
    })
    .expect("with_backend kernel")
}

#[rstest]
#[case::once("pathprobe --path a", "path=a")]
#[case::twice("pathprobe --path a --path b", "path=a\npath=b")]
#[case::equals_form("pathprobe --path=a --path=b", "path=a\npath=b")]
#[case::mixed_forms("pathprobe --path=a --path b", "path=a\npath=b")]
#[case::space_in_value("pathprobe --path 'x y' --path z", "path=x y\npath=z")]
#[case::leading_dash("x='-z'; pathprobe --path $x --path a", "path=-z\npath=a")]
#[case::short_alias_and_both_long_forms("pathprobe -p a --path=b --path c", "path=a\npath=b\npath=c")]
#[case::an_array_valued_occurrence_is_one_value(
    r#"pathprobe --path $(fromjson '["a","b"]') --path c"#,
    "path=[\"a\",\"b\"]\npath=c"
)]
#[case::a_non_repeatable_flag_given_twice_keeps_the_last("pathprobe --one a --one b", "one=b")]
#[case::with_a_scalar_flag("pathprobe --path a --one b --path c", "path=a\npath=c\none=b")]
#[tokio::test]
async fn repeatable_flag_reaches_the_clap_field_one_value_per_occurrence(
    #[case] script: &str,
    #[case] expected: &str,
) {
    let result = kernel().execute(script).await.expect("execute");
    assert_eq!(result.code, 0, "{script}: {}", result.err);
    assert_eq!(result.text_out().trim_end(), expected, "{script}");
}

/// The control: a JSON array bound to a flag that is NOT repeatable is one
/// value, and still renders as its JSON text. Only the repeatable marker
/// may split an array into occurrences.
#[tokio::test]
async fn a_json_array_on_a_non_repeatable_flag_stays_one_value() {
    let result = kernel()
        .execute(r#"pathprobe --one $(fromjson '["a","b"]')"#)
        .await
        .expect("execute");
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(result.text_out().trim_end(), r#"one=["a","b"]"#);
}

/// `jq --arg NAME VALUE` is repeatable and takes two values per occurrence.
/// Its occurrences are inner arrays, which `to_argv` renders on its own; the
/// one-value marker must not claim them.
#[tokio::test]
async fn a_two_value_repeatable_flag_is_unchanged() {
    let result = kernel()
        .execute(r#"jq -n -r --arg x 1 --arg y 2 '$x + $y'"#)
        .await
        .expect("execute");
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(result.text_out().trim_end(), "12");
}

/// sed reads its expressions off the raw args; both spellings must land
/// under the same key, in order.
#[tokio::test]
async fn sed_keeps_both_expression_spellings_in_order() {
    let result = kernel()
        .execute(r#"echo ac | sed -e 's/a/b/' --expression='s/c/d/'"#)
        .await
        .expect("execute");
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(result.text_out().trim_end(), "bd");
}
