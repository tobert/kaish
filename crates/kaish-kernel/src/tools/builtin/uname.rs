//! uname — Print system identification.
//!
//! Reports **kaish** as the system identity by default. Scripts can detect
//! they're running in kaish and adapt. Use `--host` to escape to the real OS.
//!
//! # Examples
//!
//! ```kaish
//! uname                          # → kaish
//! uname -a                       # → kaish myhost 0.1.0 ... x86_64 Kaijutsu
//! uname -snm                     # → kaish myhost x86_64
//! uname --host                   # → Linux (or whatever the real OS is)
//! uname --host -a                # → Linux myhost 6.x.y ... x86_64 GNU/Linux
//! ```

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Uname tool: print system identification.
pub struct Uname;

/// All six uname fields in GNU order.
struct UnameInfo {
    sysname: String,
    nodename: String,
    release: String,
    version: String,
    machine: String,
    os: String,
}

impl UnameInfo {
    /// Kaish identity — the shell reports itself as the system.
    fn kaish() -> Self {
        let version = env!("CARGO_PKG_VERSION");
        let git_hash = option_env!("KAISH_GIT_HASH").unwrap_or("unknown");
        let build_date = option_env!("KAISH_BUILD_DATE").unwrap_or("unknown");

        Self {
            sysname: "kaish".to_string(),
            nodename: read_hostname(),
            release: version.to_string(),
            version: format!("kaish {version} ({git_hash} {build_date})"),
            machine: std::env::consts::ARCH.to_string(),
            os: "Kaijutsu".to_string(),
        }
    }

    /// Host OS identity — reads from /proc on Linux.
    fn host() -> Self {
        let read = |name: &str| {
            std::fs::read_to_string(format!("/proc/sys/kernel/{name}"))
                .map(|s| s.trim().to_string())
                .unwrap_or_else(|_| "unknown".to_string())
        };

        Self {
            sysname: read("ostype"),
            nodename: read_hostname(),
            release: read("osrelease"),
            version: read("version"),
            machine: std::env::consts::ARCH.to_string(),
            os: "GNU/Linux".to_string(),
        }
    }
}

/// Read the hostname from /proc/sys/kernel/hostname.
pub(super) fn read_hostname() -> String {
    std::fs::read_to_string("/proc/sys/kernel/hostname")
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|_| "unknown".to_string())
}

#[async_trait]
impl Tool for Uname {
    fn name(&self) -> &str {
        "uname"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("uname", "Print system identification")
            .param(ParamSchema::optional(
                "s",
                "bool",
                Value::Bool(false),
                "Print the system name (default)",
            ))
            .param(ParamSchema::optional(
                "n",
                "bool",
                Value::Bool(false),
                "Print the network node hostname",
            ))
            .param(ParamSchema::optional(
                "r",
                "bool",
                Value::Bool(false),
                "Print the kernel release",
            ))
            .param(ParamSchema::optional(
                "v",
                "bool",
                Value::Bool(false),
                "Print the kernel version string",
            ))
            .param(ParamSchema::optional(
                "m",
                "bool",
                Value::Bool(false),
                "Print the machine hardware name",
            ))
            .param(ParamSchema::optional(
                "o",
                "bool",
                Value::Bool(false),
                "Print the operating system",
            ))
            .param(ParamSchema::optional(
                "p",
                "bool",
                Value::Bool(false),
                "Print the processor type (alias for -m)",
            ))
            .param(ParamSchema::optional(
                "i",
                "bool",
                Value::Bool(false),
                "Print the hardware platform (alias for -m)",
            ))
            .param(ParamSchema::optional(
                "a",
                "bool",
                Value::Bool(false),
                "Print all information",
            ))
            .param(ParamSchema::optional(
                "host",
                "bool",
                Value::Bool(false),
                "Report host OS identity instead of kaish",
            ))
            .example("System name", "uname")
            .example("All info", "uname -a")
            .example("Host OS identity", "uname --host")
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        let host_mode = args.has_flag("host");
        let info = if host_mode {
            UnameInfo::host()
        } else {
            UnameInfo::kaish()
        };

        let all = args.has_flag("a");

        // Collect which fields to print
        let flag_s = all || args.has_flag("s");
        let flag_n = all || args.has_flag("n");
        let flag_r = all || args.has_flag("r");
        let flag_v = all || args.has_flag("v");
        let flag_m = all || args.has_flag("m") || args.has_flag("p") || args.has_flag("i");
        let flag_o = all || args.has_flag("o");

        // If no flags, default to -s
        let any_explicit = flag_s || flag_n || flag_r || flag_v || flag_m || flag_o;

        let mut parts: Vec<&str> = Vec::new();

        if flag_s || !any_explicit {
            parts.push(&info.sysname);
        }
        if flag_n {
            parts.push(&info.nodename);
        }
        if flag_r {
            parts.push(&info.release);
        }
        if flag_v {
            parts.push(&info.version);
        }
        if flag_m {
            parts.push(&info.machine);
        }
        if flag_o {
            parts.push(&info.os);
        }

        let output = parts.join(" ");
        ExecResult::with_output(OutputData::text(output))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_default_is_sysname() {
        let mut ctx = make_ctx();
        let result = Uname.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "kaish");
    }

    #[tokio::test]
    async fn test_flag_s() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("s".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "kaish");
    }

    #[tokio::test]
    async fn test_flag_n() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("n".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should be non-empty hostname
        assert!(!result.out.is_empty());
    }

    #[tokio::test]
    async fn test_flag_r() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("r".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, env!("CARGO_PKG_VERSION"));
    }

    #[tokio::test]
    async fn test_flag_v() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("v".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.starts_with("kaish "));
        assert!(result.out.contains(env!("CARGO_PKG_VERSION")));
    }

    #[tokio::test]
    async fn test_flag_m() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("m".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, std::env::consts::ARCH);
    }

    #[tokio::test]
    async fn test_flag_o() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("o".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "Kaijutsu");
    }

    #[tokio::test]
    async fn test_processor_aliases_machine() {
        let mut ctx = make_ctx();

        // -p should match -m
        let mut args_p = ToolArgs::new();
        args_p.flags.insert("p".to_string());
        let result_p = Uname.execute(args_p, &mut ctx).await;

        let mut args_m = ToolArgs::new();
        args_m.flags.insert("m".to_string());
        let result_m = Uname.execute(args_m, &mut ctx).await;

        assert_eq!(result_p.out, result_m.out);

        // -i should match -m
        let mut args_i = ToolArgs::new();
        args_i.flags.insert("i".to_string());
        let result_i = Uname.execute(args_i, &mut ctx).await;
        assert_eq!(result_i.out, result_m.out);
    }

    #[tokio::test]
    async fn test_all_flag() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("a".to_string());

        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());

        let fields: Vec<&str> = result.out.split_whitespace().collect();
        // -a produces: sysname nodename release version(multi-word) machine os
        // At minimum 6 whitespace-separated tokens (version has parens with spaces)
        assert!(fields.len() >= 6, "expected ≥6 fields, got: {:?}", fields);
        assert_eq!(fields[0], "kaish");
        assert_eq!(*fields.last().unwrap(), "Kaijutsu");
    }

    #[tokio::test]
    async fn test_multiple_flags() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("s".to_string());
        args.flags.insert("n".to_string());

        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());

        let parts: Vec<&str> = result.out.split(' ').collect();
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0], "kaish");
    }

    #[tokio::test]
    async fn test_host_mode() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("host".to_string());

        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        // In host mode, sysname should NOT be "kaish"
        assert_ne!(result.out, "kaish", "host mode should return real OS");
        // On Linux it should be "Linux"
        assert_eq!(result.out, "Linux");
    }

    #[tokio::test]
    async fn test_host_all() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("host".to_string());
        args.flags.insert("a".to_string());

        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());

        let fields: Vec<&str> = result.out.split_whitespace().collect();
        assert_eq!(fields[0], "Linux");
        assert_eq!(*fields.last().unwrap(), "GNU/Linux");
    }
}
