//! Build script for Cap'n Proto schema compilation.
//!
//! Compiles schema/kaish.capnp into Rust types at build time.

#[allow(clippy::expect_used)]
fn main() {
    println!("cargo::rerun-if-changed=schema/kaish.capnp");

    capnpc::CompilerCommand::new()
        .src_prefix("schema")
        .file("schema/kaish.capnp")
        .run()
        .expect("capnp schema compilation failed");
}
