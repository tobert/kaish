//! jq — Native JSON query tool using jaq.
//!
//! This implementation uses the jaq crate for native jq execution
//! without spawning an external process. Benefits:
//! - Parse-time filter validation (fail fast)
//! - No subprocess overhead
//! - Consistent behavior across platforms
//! - Type-safe Rust API
//!
//! # Examples
//!
//! ```kaish
//! echo '{"name": "Alice"}' | jq ".name"
//! echo '{"name": "Alice"}' | jq ".name" -r
//! jq ".items[]" path=/data/items.json
//! jq ".[] | select(.active)" -c
//! ```

use std::path::Path;

use async_trait::async_trait;
use jaq_core::{load, compile, Ctx, RcIter};
use jaq_json::Val;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Native jq tool using jaq (pure Rust jq implementation).
pub struct JqNative;

type Filter = jaq_core::Filter<jaq_core::Native<Val>>;

/// Parse and compile a jq filter expression.
fn compile_filter(filter_str: &str) -> Result<Filter, String> {
    // Create arena for parsing
    let arena = load::Arena::default();

    // Load standard library definitions
    let defs = jaq_std::defs().chain(jaq_json::defs());
    let loader = load::Loader::new(defs);

    // Parse the filter
    let modules = loader
        .load(&arena, load::File { path: (), code: filter_str })
        .map_err(|errs| {
            let msgs: Vec<String> = errs
                .into_iter()
                .flat_map(|(_, e)| -> Vec<String> {
                    match e {
                        load::Error::Io(io_errs) => io_errs.into_iter().map(|(_, msg)| msg).collect(),
                        load::Error::Lex(lex_errs) => lex_errs.into_iter().map(|(expected, _)| format!("expected {}", expected.as_str())).collect(),
                        load::Error::Parse(parse_errs) => parse_errs.into_iter().map(|(expected, _)| format!("expected {}", expected.as_str())).collect(),
                    }
                })
                .collect();
            format!("jq parse error: {}", msgs.join(", "))
        })?;

    // Compile with standard library functions
    let funs = jaq_std::funs().chain(jaq_json::funs());
    let compiler = compile::Compiler::default().with_funs(funs);
    let filter = compiler.compile(modules).map_err(|errs| {
        let msgs: Vec<String> = errs
            .into_iter()
            .flat_map(|(_, errors)| {
                errors.into_iter().map(|(_, undefined)| format!("undefined {}", undefined.as_str()))
            })
            .collect();
        format!("jq compile error: {}", msgs.join(", "))
    })?;

    Ok(filter)
}

/// Execute a compiled jq filter on JSON input.
fn execute_filter(filter: &Filter, input: &str, raw_output: bool) -> Result<String, String> {
    // Parse input JSON
    let json: serde_json::Value = serde_json::from_str(input)
        .map_err(|e| format!("jq: invalid JSON input: {}", e))?;

    // Convert serde_json::Value to jaq_json::Val
    let input_val = json_to_val(json);

    // Create execution context with empty inputs iterator
    let inputs: RcIter<_> = RcIter::new(Box::new(core::iter::empty()));
    let ctx = Ctx::new(Vec::new(), &inputs);

    // Run the filter
    let results: Vec<Result<Val, jaq_core::Error<Val>>> = filter
        .run((ctx, input_val))
        .collect();

    // Format output
    let mut output = String::new();
    for result in results {
        match result {
            Ok(val) => {
                let formatted = if raw_output {
                    format_raw(&val)
                } else {
                    format_json(&val)
                };
                if !output.is_empty() {
                    output.push('\n');
                }
                output.push_str(&formatted);
            }
            Err(e) => {
                return Err(format!("jq runtime error: {}", e));
            }
        }
    }

    Ok(output)
}

/// Convert serde_json::Value to jaq_json::Val.
fn json_to_val(json: serde_json::Value) -> Val {
    use std::rc::Rc;
    match json {
        serde_json::Value::Null => Val::Null,
        serde_json::Value::Bool(b) => Val::Bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                // Try to fit in isize, fall back to Num for large values
                if let Ok(i) = isize::try_from(i) {
                    Val::Int(i)
                } else {
                    Val::Num(Rc::new(n.to_string()))
                }
            } else if let Some(f) = n.as_f64() {
                Val::Float(f)
            } else {
                // Fall back to string representation for very large numbers
                Val::Num(Rc::new(n.to_string()))
            }
        }
        serde_json::Value::String(s) => Val::Str(Rc::new(s)),
        serde_json::Value::Array(arr) => Val::Arr(Rc::new(arr.into_iter().map(json_to_val).collect())),
        serde_json::Value::Object(obj) => {
            Val::obj(obj.into_iter().map(|(k, v)| (Rc::new(k), json_to_val(v))).collect())
        }
    }
}

/// Format a jaq value as raw output (strings without quotes).
fn format_raw(val: &Val) -> String {
    match val {
        Val::Str(s) => s.to_string(),
        Val::Null => "null".to_string(),
        Val::Bool(b) => b.to_string(),
        Val::Int(n) => n.to_string(),
        Val::Float(n) => format!("{:?}", n),
        Val::Num(s) => s.to_string(),
        Val::Arr(arr) => {
            let items: Vec<String> = arr.iter().map(format_raw).collect();
            items.join("\n")
        }
        Val::Obj(_) => serde_json::to_string(&val_to_json(val)).unwrap_or_default(),
    }
}

/// Format a jaq value as JSON.
fn format_json(val: &Val) -> String {
    serde_json::to_string_pretty(&val_to_json(val)).unwrap_or_default()
}

/// Convert jaq Val to serde_json Value.
fn val_to_json(val: &Val) -> serde_json::Value {
    match val {
        Val::Null => serde_json::Value::Null,
        Val::Bool(b) => serde_json::Value::Bool(*b),
        Val::Int(n) => serde_json::Value::Number((*n as i64).into()),
        Val::Float(n) => serde_json::Number::from_f64(*n)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        Val::Num(s) => {
            // Parse the string number back to a JSON number
            serde_json::from_str(s).unwrap_or(serde_json::Value::String(s.to_string()))
        }
        Val::Str(s) => serde_json::Value::String(s.to_string()),
        Val::Arr(arr) => serde_json::Value::Array(arr.iter().map(val_to_json).collect()),
        Val::Obj(obj) => {
            let map: serde_json::Map<String, serde_json::Value> = obj
                .iter()
                .map(|(k, v)| (k.to_string(), val_to_json(v)))
                .collect();
            serde_json::Value::Object(map)
        }
    }
}

#[async_trait]
impl Tool for JqNative {
    fn name(&self) -> &str {
        "jq"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("jq", "JSON query processor (native jaq implementation)")
            .param(ParamSchema::required(
                "filter",
                "string",
                "jq filter expression",
            ))
            .param(ParamSchema::optional(
                "raw",
                "bool",
                Value::Bool(false),
                "Raw output mode (-r): output strings without quotes",
            ))
            .param(ParamSchema::optional(
                "compact",
                "bool",
                Value::Bool(false),
                "Compact output mode (-c): no pretty-printing",
            ))
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::String("".into()),
                "Read from VFS file instead of stdin",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Get filter (required, positional 0)
        let filter_str = match args.get_string("filter", 0) {
            Some(f) => f,
            None => return ExecResult::failure(1, "jq: filter expression required"),
        };

        // Compile filter (validates at execution time)
        let filter = match compile_filter(&filter_str) {
            Ok(f) => f,
            Err(e) => return ExecResult::failure(1, e),
        };

        let raw_output = args.has_flag("raw") || args.has_flag("r");
        let _compact = args.has_flag("compact") || args.has_flag("c");

        // Get input: from path or stdin
        let input = if let Some(path) = args.get_string("path", 999) {
            if !path.is_empty() {
                // Read from backend
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
                    Err(e) => {
                        return ExecResult::failure(1, format!("jq: failed to read {}: {}", path, e))
                    }
                }
            } else {
                ctx.take_stdin().unwrap_or_default()
            }
        } else {
            ctx.take_stdin().unwrap_or_default()
        };

        if input.is_empty() {
            return ExecResult::failure(1, "jq: no input provided");
        }

        // Execute filter
        match execute_filter(&filter, &input, raw_output) {
            Ok(output) => ExecResult::success(output),
            Err(e) => ExecResult::failure(1, e),
        }
    }
}

/// Validate a jq filter expression without executing it.
/// Returns Ok(()) if valid, Err(message) if invalid.
#[allow(dead_code)] // Used by tests, will be used by parser for early validation
pub fn validate_filter(filter: &str) -> Result<(), String> {
    compile_filter(filter)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::WriteMode;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[test]
    fn test_validate_filter_valid() {
        assert!(validate_filter(".name").is_ok());
        assert!(validate_filter(".items[]").is_ok());
        assert!(validate_filter(".[] | select(.active)").is_ok());
        assert!(validate_filter("map(.x + 1)").is_ok());
    }

    #[test]
    fn test_validate_filter_invalid() {
        assert!(validate_filter(".[[[invalid").is_err());
        assert!(validate_filter(".foo | | bar").is_err());
    }

    #[tokio::test]
    async fn test_jq_native_simple_filter() {
        let mut ctx = make_ctx();
        ctx.set_stdin(r#"{"name": "Alice"}"#.to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".name".into()));

        let result = JqNative.execute(args, &mut ctx).await;
        assert!(result.ok(), "jq failed: {}", result.err);
        assert_eq!(result.out.trim(), "\"Alice\"");
    }

    #[tokio::test]
    async fn test_jq_native_raw_output() {
        let mut ctx = make_ctx();
        ctx.set_stdin(r#"{"name": "Alice"}"#.to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".name".into()));
        args.flags.insert("r".to_string());

        let result = JqNative.execute(args, &mut ctx).await;
        assert!(result.ok(), "jq failed: {}", result.err);
        assert_eq!(result.out.trim(), "Alice");
    }

    #[tokio::test]
    async fn test_jq_native_array_iteration() {
        let mut ctx = make_ctx();
        ctx.set_stdin(r#"[1, 2, 3]"#.to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".[]".into()));

        let result = JqNative.execute(args, &mut ctx).await;
        assert!(result.ok(), "jq failed: {}", result.err);
        assert_eq!(result.out.trim(), "1\n2\n3");
    }

    #[tokio::test]
    async fn test_jq_native_invalid_json() {
        let mut ctx = make_ctx();
        ctx.set_stdin("not valid json".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".".into()));

        let result = JqNative.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("invalid JSON"));
    }

    #[tokio::test]
    async fn test_jq_native_invalid_filter() {
        let mut ctx = make_ctx();
        ctx.set_stdin(r#"{"a": 1}"#.to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".[[[invalid".into()));

        let result = JqNative.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_jq_native_no_input() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".".into()));

        let result = JqNative.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("no input"));
    }

    #[tokio::test]
    async fn test_jq_native_from_vfs_file() {
        let mut ctx = make_ctx();

        // Write test data to backend
        ctx.backend
            .write(Path::new("/test.json"), br#"{"value": 42}"#, WriteMode::Overwrite)
            .await
            .expect("failed to write test file");

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".value".into()));
        args.named
            .insert("path".to_string(), Value::String("/test.json".into()));

        let result = JqNative.execute(args, &mut ctx).await;
        assert!(result.ok(), "jq failed: {}", result.err);
        assert_eq!(result.out.trim(), "42");
    }
}
