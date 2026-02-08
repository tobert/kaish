//! Build script for Cap'n Proto schema compilation.
//!
//! When the `codegen` feature is enabled, compiles schema/kaish.capnp into
//! Rust types at build time (requires the `capnp` CLI tool).
//!
//! Without `codegen`, the vendored src/kaish_capnp.rs is used directly —
//! no external tools needed.

fn main() {
    #[cfg(feature = "codegen")]
    {
        #[allow(clippy::expect_used)]
        capnpc::CompilerCommand::new()
            .src_prefix("schema")
            .file("schema/kaish.capnp")
            .run()
            .expect("capnp schema compilation failed");
    }
}
