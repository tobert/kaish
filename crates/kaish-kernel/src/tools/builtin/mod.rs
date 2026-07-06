//! Built-in tools for kaish.
//!
//! These tools are always available and provide core functionality.

mod alias;
mod assert;
mod awk;
mod base64_tool;
mod basename;
#[cfg(feature = "subprocess")]
mod bg;
mod cat;
mod cd;
mod checksum;
mod cmp;
mod cp;
mod cut;
mod date;
mod dd;
mod diff;
mod dirname;
mod patch;
mod echo;
mod env;
#[cfg(feature = "subprocess")]
mod exec;
#[cfg(feature = "subprocess")]
mod spawn;
#[cfg(feature = "subprocess")]
pub use spawn::resolve_in_path;
mod export;
#[cfg(feature = "subprocess")]
mod fg;
mod file;
mod fromjson;
mod fromjsonl;
mod glob;
mod find;
pub(crate) mod format_string;
mod gather;
mod grep;
mod grep_engine;
mod regex_dialect;
mod head;
mod ignore;
mod help;
// `hostname` is pure host introspection (reads /proc) — gated behind the host
// capability. `uname` stays available and reports kaish identity instead.
#[cfg(feature = "host")]
mod hostname;
mod introspect;
mod jobs;
mod jq_native;
mod kaish_ast;
mod kaish_clear;
mod kaish_last;
mod kaish_trash;
mod kaish_status;
mod kaish_version;
mod kaish_vfs;
mod keys;
mod kill;
mod ln;
mod ls;
mod mkdir;
mod mktemp;
mod mv;
mod output_limit;
mod printf;
mod push;
mod pwd;
mod read;
mod readlink;
mod realpath;
mod rm;
mod scatter;
mod sed;
mod seq;
mod split;
mod set;
mod sleep;
mod sort;
mod stat;
mod tac;
mod tail;
mod tee;
mod test;
mod timeout;
mod tojson;
mod tojsonl;
#[cfg(feature = "tokens")]
mod tokens;
mod touch;
mod tr;
mod tree;
mod true_false;
// Module named `type_of`, not `typeof` — `typeof` is a reserved (but unused)
// Rust keyword, so `mod typeof;` doesn't compile. The tool name is still the
// plain string "typeof" (see `Tool::name`).
mod type_of;
mod uname;
mod uniq;
mod unset;
mod validate;
mod values;
mod vars;
mod wait;
mod wc;
#[cfg(feature = "subprocess")]
mod which;
mod write;
mod xxd;

use super::ToolRegistry;

/// Read a repeatable string-valued flag off the raw `ToolArgs`.
///
/// Repeatable value flags (clap `Append`) are accumulated by the kernel into a
/// `Value::Json(Array)` under the flag's long name; a single occurrence may
/// arrive as a bare `Value::String`. `to_argv()` can't round-trip the array, so
/// the clap field isn't a reliable source — search builtins read the raw args
/// here. Shared by grep/glob `--ftype`/`--ftype-not`; mirrors sed's
/// `collect_expressions` (see the repeatable-flag gotcha in `arch_repeatable_flags`).
pub(crate) fn read_repeatable_strings(args: &super::ToolArgs, key: &str) -> Vec<String> {
    use crate::ast::Value;
    match args.named.get(key) {
        Some(Value::Json(serde_json::Value::Array(items))) => items
            .iter()
            .filter_map(|v| v.as_str().map(|s| s.to_string()))
            .collect(),
        Some(Value::String(s)) => vec![s.clone()],
        Some(other) => vec![crate::interpreter::value_to_string(other)],
        None => Vec::new(),
    }
}

/// Read a path-typed positional/named arg as a string, going LOUD on a
/// `Value::Bytes` operand rather than `ToolArgs::get_string`'s silent `None`
/// (that method lives in the `kaish-types` leaf crate, which has no
/// `EvalError`/rich-error machinery to report *why* it returned nothing, so it
/// just treats any non-scalar-text value as absent).
///
/// A caller reading `get_string` for a *path* generally treats `None` as
/// "operand missing" and either errors or falls back to some other input
/// (stdin, `$HOME`, …) — exactly the silent-wrong-source shape GH #93 item 1
/// is about, just reached through `get_string`'s catch-all instead of
/// `value_to_string`'s placeholder. `Ok(None)` here means genuinely absent;
/// `Err` means it was present and binary.
pub(crate) fn get_path_string(
    args: &super::ToolArgs,
    name: &str,
    positional_index: usize,
) -> Result<Option<String>, String> {
    use crate::ast::Value;
    match args.get(name, positional_index) {
        Some(v @ Value::Bytes(_)) => match crate::interpreter::value_to_text_sink_named(v, "a path") {
            Ok(s) => Ok(Some(s)),
            Err(e) => Err(e.to_string()),
        },
        _ => Ok(args.get_string(name, positional_index)),
    }
}

/// Register all built-in tools with the registry.
pub fn register_builtins(registry: &mut ToolRegistry) {
    registry.register(alias::Alias);
    registry.register(alias::Unalias);
    registry.register(assert::Assert);
    registry.register(awk::Awk);
    registry.register(base64_tool::Base64Tool);
    registry.register(basename::Basename);
    #[cfg(feature = "subprocess")]
    registry.register(bg::Bg);
    registry.register(cat::Cat);
    registry.register(cd::Cd);
    registry.register(checksum::Checksum);
    registry.register(cmp::Cmp);
    registry.register(cp::Cp);
    registry.register(cut::Cut);
    registry.register(date::Date::new());
    registry.register(dd::Dd);
    registry.register(diff::Diff);
    registry.register(dirname::Dirname);
    registry.register(echo::Echo);
    registry.register(env::Env);
    #[cfg(feature = "subprocess")]
    registry.register(exec::Exec);
    #[cfg(feature = "subprocess")]
    registry.register(spawn::Spawn);
    registry.register(export::Export);
    #[cfg(feature = "subprocess")]
    registry.register(fg::Fg);
    registry.register(file::File);
    registry.register(fromjson::FromJson);
    registry.register(fromjsonl::FromJsonl);
    registry.register(glob::Glob);
    registry.register(find::Find);
    registry.register(gather::Gather);
    registry.register(grep::Grep);
    registry.register(head::Head);
    registry.register(help::Help);
    registry.register(ignore::KaishIgnore);
    #[cfg(feature = "host")]
    registry.register(hostname::Hostname);
    registry.register(introspect::Mounts);
    registry.register(introspect::Tools);
    registry.register(jobs::Jobs);
    registry.register(jq_native::JqNative);
    registry.register(kaish_ast::KaishAst);
    registry.register(kaish_clear::KaishClear);
    registry.register(kaish_last::KaishLast);
    registry.register(kaish_trash::KaishTrash);
    registry.register(kaish_status::KaishStatus);
    registry.register(kaish_version::KaishVersion);
    registry.register(kaish_vfs::KaishVfs);
    registry.register(keys::Keys);
    registry.register(kill::Kill);
    registry.register(ln::Ln);
    registry.register(ls::Ls);
    registry.register(mkdir::Mkdir);
    registry.register(mktemp::Mktemp);
    registry.register(mv::Mv);
    registry.register(output_limit::KaishOutputLimit);
    registry.register(patch::Patch);
    registry.register(printf::Printf);
    registry.register(push::Push);
    #[cfg(all(target_os = "linux", feature = "host"))]
    registry.register(kaish_tools_host::Ps);
    registry.register(pwd::Pwd);
    registry.register(read::Read);
    registry.register(readlink::Readlink);
    registry.register(realpath::Realpath);
    registry.register(rm::Rm);
    registry.register(scatter::Scatter);
    registry.register(sed::Sed);
    registry.register(seq::Seq);
    registry.register(set::Set);
    registry.register(split::Split);
    registry.register(sleep::Sleep);
    registry.register(sort::Sort);
    registry.register(stat::Stat);
    registry.register(tac::Tac);
    registry.register(tail::Tail);
    registry.register(tee::Tee);
    registry.register(test::Test);
    registry.register(timeout::Timeout);
    #[cfg(feature = "tokens")]
    registry.register(tokens::Tokens);
    registry.register(tojson::ToJson);
    registry.register(tojsonl::ToJsonl);
    registry.register(touch::Touch);
    registry.register(tr::Tr);
    registry.register(tree::Tree);
    registry.register(true_false::True);
    registry.register(true_false::False);
    registry.register(type_of::TypeOf);
    registry.register(uname::Uname);
    registry.register(uniq::Uniq);
    registry.register(unset::Unset);
    registry.register(validate::Validate);
    registry.register(values::Values);
    registry.register(vars::Vars);
    registry.register(wait::Wait);
    registry.register(wc::Wc);
    #[cfg(feature = "subprocess")]
    registry.register(which::Which);
    registry.register(write::Write);
    registry.register(xxd::Xxd);
}
