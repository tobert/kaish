//! uname — Print system identification.
//!
//! Reports **kaish** as the system identity by default. Scripts can detect
//! they're running in kaish and adapt. The node name comes from the exported
//! `HOSTNAME` variable (frontend-supplied, like the rest of the hermetic env),
//! falling back to `kaish` — the kernel never reads the host's real hostname in
//! this mode.
//!
//! `--host` escapes to the real OS by reading `/proc`. That is a host-capability
//! and is only compiled in with the `native` feature; in a minimal/hermetic
//! build `--host` reports an error rather than silently doing nothing.
//!
//! Note the deliberate POSIX divergence: when `HOSTNAME` is unset, `uname -n`
//! reports `kaish`, whereas the `hostname` builtin (present only in `native`
//! builds) reports the real host name. The two are *not* synonyms here.
//!
//! # Examples
//!
//! ```kaish
//! uname                          # → kaish
//! uname -a                       # → kaish myhost 0.1.0 ... x86_64 kai
//! uname -snm                     # → kaish myhost x86_64
//! uname --host                   # → Linux (native build only)
//! uname --host -a                # → Linux myhost 6.x.y ... x86_64 GNU/Linux
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Uname tool: print system identification.
pub struct Uname;

/// clap-derived argv layer for uname.
#[derive(Parser, Debug)]
#[command(name = "uname", about = "Print system identification")]
struct UnameArgs {
    /// Print the system name (default).
    #[arg(id = "s", short = 's', long = "s")]
    _s: bool,

    /// Print the network node hostname.
    #[arg(id = "n", short = 'n', long = "n")]
    _n: bool,

    /// Print the kernel release.
    #[arg(id = "r", short = 'r', long = "r")]
    _r: bool,

    /// Print the kernel version string.
    #[arg(id = "v", short = 'v', long = "v")]
    _v: bool,

    /// Print the machine hardware name.
    #[arg(id = "m", short = 'm', long = "m")]
    _m: bool,

    /// Print the operating system.
    #[arg(id = "o", short = 'o', long = "o")]
    _o: bool,

    /// Print the processor type (alias for -m).
    #[arg(id = "p", short = 'p', long = "p")]
    _p: bool,

    /// Print the hardware platform (alias for -m).
    #[arg(id = "i", short = 'i', long = "i")]
    _i: bool,

    /// Print all information.
    #[arg(id = "a", short = 'a', long = "a")]
    _a: bool,

    /// Report host OS identity instead of kaish.
    ///
    /// Hidden from the schema in non-`native` builds (the capability isn't
    /// compiled in) while still parsed, so `uname --host` returns the curated
    /// "unavailable" error instead of an unknown-argument complaint.
    #[arg(id = "host", long = "host")]
    #[cfg_attr(not(feature = "host"), arg(hide = true))]
    _host: bool,

    #[command(flatten)]
    global: GlobalFlags,
}

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
    ///
    /// `nodename` is supplied by the caller (the exported `HOSTNAME` var, or
    /// the `kaish` default); we never touch `/proc` here.
    fn kaish(nodename: String) -> Self {
        let version = env!("CARGO_PKG_VERSION");
        let git_hash = option_env!("KAISH_GIT_HASH").unwrap_or("unknown");
        let build_date = option_env!("KAISH_BUILD_DATE").unwrap_or("unknown");

        Self {
            sysname: "kaish".to_string(),
            nodename,
            release: version.to_string(),
            version: format!("kaish {version} ({git_hash} {build_date})"),
            machine: std::env::consts::ARCH.to_string(),
            os: "kai".to_string(),
        }
    }

    /// Host OS identity — reads from `/proc` on Linux.
    ///
    /// Host introspection is a capability gated behind the `native` feature;
    /// the minimal/hermetic build has no path to the real host here.
    #[cfg(feature = "host")]
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

/// Kaish-mode node name: the exported `HOSTNAME` variable, or `kaish` when it
/// is unset/empty. Deliberately does not read the host — frontends seed
/// `HOSTNAME` via `initial_vars` when they want the real value exposed.
fn kaish_nodename(ctx: &ExecContext) -> String {
    match ctx.scope.get("HOSTNAME") {
        Some(Value::String(name)) if !name.is_empty() => name.clone(),
        _ => "kaish".to_string(),
    }
}

/// Read the real hostname from `/proc/sys/kernel/hostname`.
///
/// Host introspection — only compiled in with the `native` feature.
#[cfg(feature = "host")]
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
        schema_from_clap(
            &UnameArgs::command(),
            "uname",
            "Print system identification",
            [
                ("System name", "uname"),
                ("All info", "uname -a"),
                ("Host OS identity", "uname --host"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("uname: {e}")),
        };
        let parsed = match UnameArgs::try_parse_from(
            std::iter::once("uname".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("uname: {e}")),
        };
        parsed.global.apply(ctx);

        let host_mode = args.has_flag("host");
        let info = if host_mode {
            #[cfg(feature = "host")]
            {
                UnameInfo::host()
            }
            #[cfg(not(feature = "host"))]
            {
                return ExecResult::failure(
                    2,
                    "uname: --host is unavailable in this build (no host capability)".to_string(),
                );
            }
        } else {
            UnameInfo::kaish(kaish_nodename(ctx))
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
        assert_eq!(&*result.text_out(), "kaish");
    }

    #[tokio::test]
    async fn test_flag_s() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("s".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "kaish");
    }

    #[tokio::test]
    async fn test_flag_n_defaults_to_kaish() {
        // With no HOSTNAME var, kaish-identity nodename is the literal "kaish" —
        // the kernel must not read the host's real hostname here.
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("n".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "kaish");
    }

    #[tokio::test]
    async fn test_flag_n_uses_hostname_var() {
        // A frontend-supplied (exported) HOSTNAME is honored as nodename.
        let mut ctx = make_ctx();
        ctx.scope.set_exported("HOSTNAME", Value::String("sandbox-01".into()));
        let mut args = ToolArgs::new();
        args.flags.insert("n".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "sandbox-01");
    }

    #[tokio::test]
    async fn test_flag_r() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("r".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), env!("CARGO_PKG_VERSION"));
    }

    #[tokio::test]
    async fn test_flag_v() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("v".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().starts_with("kaish "));
        assert!(result.text_out().contains(env!("CARGO_PKG_VERSION")));
    }

    #[tokio::test]
    async fn test_flag_m() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("m".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), std::env::consts::ARCH);
    }

    #[tokio::test]
    async fn test_flag_o() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("o".to_string());
        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "kai");
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

        assert_eq!(&*result_p.text_out(), &*result_m.text_out());

        // -i should match -m
        let mut args_i = ToolArgs::new();
        args_i.flags.insert("i".to_string());
        let result_i = Uname.execute(args_i, &mut ctx).await;
        assert_eq!(&*result_i.text_out(), &*result_m.text_out());
    }

    #[tokio::test]
    async fn test_all_flag() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("a".to_string());

        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());

        let text = result.text_out();
        let fields: Vec<&str> = text.split_whitespace().collect();
        // -a produces: sysname nodename release version(multi-word) machine os
        // At minimum 6 whitespace-separated tokens (version has parens with spaces)
        assert!(fields.len() >= 6, "expected ≥6 fields, got: {:?}", fields);
        assert_eq!(fields[0], "kaish");
        assert_eq!(*fields.last().unwrap(), "kai");
    }

    #[tokio::test]
    async fn test_multiple_flags() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("s".to_string());
        args.flags.insert("n".to_string());

        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());

        let text = result.text_out();
        let parts: Vec<&str> = text.split(' ').collect();
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0], "kaish");
    }

    #[cfg(all(target_os = "linux", feature = "host"))]
    #[tokio::test]
    async fn test_host_mode() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("host".to_string());

        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());
        // In host mode, sysname should NOT be "kaish"
        assert_ne!(&*result.text_out(), "kaish", "host mode should return real OS");
        // On Linux it should be "Linux"
        assert_eq!(&*result.text_out(), "Linux");
    }

    #[cfg(all(target_os = "linux", feature = "host"))]
    #[tokio::test]
    async fn test_host_all() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("host".to_string());
        args.flags.insert("a".to_string());

        let result = Uname.execute(args, &mut ctx).await;
        assert!(result.ok());

        let text = result.text_out();
        let fields: Vec<&str> = text.split_whitespace().collect();
        assert_eq!(fields[0], "Linux");
        assert_eq!(*fields.last().unwrap(), "GNU/Linux");
    }

    /// Without the host capability, `--host` must fail loudly rather than
    /// silently report kaish identity.
    #[cfg(not(feature = "host"))]
    #[tokio::test]
    async fn test_host_unavailable_without_native() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("host".to_string());

        let result = Uname.execute(args, &mut ctx).await;
        assert!(!result.ok(), "--host must error without the host capability");
    }
}
