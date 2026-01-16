//! Built-in tools for kaish.
//!
//! These tools are always available and provide core functionality.

mod assert;
mod cat;
mod cd;
mod cp;
mod date;
mod echo;
mod grep;
mod help;
mod jobs;
mod ls;
mod mkdir;
mod mv;
mod pwd;
mod rm;
mod wait;
mod write;

use super::ToolRegistry;

/// Register all built-in tools with the registry.
pub fn register_builtins(registry: &mut ToolRegistry) {
    registry.register(assert::Assert);
    registry.register(cat::Cat);
    registry.register(cd::Cd);
    registry.register(cp::Cp);
    registry.register(date::Date);
    registry.register(echo::Echo);
    registry.register(grep::Grep);
    registry.register(help::Help);
    registry.register(jobs::Jobs);
    registry.register(ls::Ls);
    registry.register(mkdir::Mkdir);
    registry.register(mv::Mv);
    registry.register(pwd::Pwd);
    registry.register(rm::Rm);
    registry.register(wait::Wait);
    registry.register(write::Write);
}
