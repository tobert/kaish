//! `command -v`, `command -V`, and `type` resolve a name the way the kernel
//! runs it: alias, function, builtin, then a program on `PATH`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::os::unix::fs::PermissionsExt;

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

/// A kernel whose `PATH` is one host directory holding an executable
/// `mytool` and a non-executable `notatool`.
async fn kernel() -> (Arc<Kernel>, tempfile::TempDir) {
    let dir = tempfile::tempdir().expect("tempdir");
    let tool = dir.path().join("mytool");
    std::fs::write(&tool, "#!/bin/sh\n").unwrap();
    std::fs::set_permissions(&tool, std::fs::Permissions::from_mode(0o755)).unwrap();
    std::fs::write(dir.path().join("notatool"), "data\n").unwrap();
    let kernel = Kernel::new(KernelConfig::isolated()).expect("isolated kernel").into_arc();
    let setup = format!(
        "PATH={}; alias ll='ls -l'; greet() {{ echo hi; }}",
        dir.path().display()
    );
    let result = kernel.execute(&setup).await.expect("setup");
    assert_eq!(result.code, 0, "setup failed: {}", result.err);
    (kernel, dir)
}

async fn run(kernel: &Kernel, script: &str) -> (String, String, i64) {
    let result = kernel.execute(script).await.expect("kernel execute");
    (result.text_out().into_owned(), result.err.clone(), result.code)
}

// Resolving a program on PATH needs the subprocess feature.
#[cfg(feature = "subprocess")]
#[tokio::test]
async fn command_v_names_each_kind() {
    let (kernel, dir) = kernel().await;
    let tool = dir.path().join("mytool").display().to_string();
    for (name, expected) in [
        ("echo", "echo".to_string()),
        ("true", "true".to_string()),
        ("greet", "greet".to_string()),
        ("ll", "alias ll='ls -l'".to_string()),
        ("mytool", tool.clone()),
    ] {
        let (out, err, code) = run(&kernel, &format!("command -v {name}")).await;
        assert_eq!((out.trim_end(), code), (expected.as_str(), 0), "{name}: {err}");
    }
}

#[tokio::test]
async fn command_v_of_a_missing_name_is_silent_and_fails() {
    let (kernel, _dir) = kernel().await;
    for name in ["nosuch", "notatool"] {
        let (out, err, code) = run(&kernel, &format!("command -v {name}")).await;
        assert_eq!((out.as_str(), err.as_str(), code), ("", "", 1), "{name}");
    }
}

// Resolving a program on PATH needs the subprocess feature.
#[cfg(feature = "subprocess")]
#[tokio::test]
async fn command_v_answers_the_missing_program_idiom() {
    let (kernel, _dir) = kernel().await;
    let (out, _, _) = run(&kernel, "command -v mytool >/dev/null || echo MISSING").await;
    assert_eq!(out, "");
    let (out, _, _) = run(&kernel, "command -v nosuch >/dev/null || echo MISSING").await;
    assert_eq!(out.trim_end(), "MISSING");
}

#[tokio::test]
async fn command_v_with_several_names_fails_if_any_is_missing() {
    let (kernel, _dir) = kernel().await;
    let (out, _, code) = run(&kernel, "command -v echo nosuch greet").await;
    assert_eq!(out.lines().collect::<Vec<_>>(), ["echo", "greet"]);
    assert_eq!(code, 1);
}

// Resolving a program on PATH needs the subprocess feature.
#[cfg(feature = "subprocess")]
#[tokio::test]
async fn type_describes_each_kind() {
    let (kernel, dir) = kernel().await;
    let tool = dir.path().join("mytool").display().to_string();
    for (name, expected) in [
        ("echo", "echo is a shell builtin".to_string()),
        ("greet", "greet is a function".to_string()),
        ("ll", "ll is aliased to `ls -l'".to_string()),
        ("mytool", format!("mytool is {tool}")),
    ] {
        let (out, err, code) = run(&kernel, &format!("type {name}")).await;
        assert_eq!((out.trim_end(), code), (expected.as_str(), 0), "{name}: {err}");
        let (out, _, _) = run(&kernel, &format!("command -V {name}")).await;
        assert_eq!(out.trim_end(), expected, "command -V {name}");
    }
}

// Resolving a program on PATH needs the subprocess feature.
#[cfg(feature = "subprocess")]
#[tokio::test]
async fn type_t_prints_one_word() {
    let (kernel, _dir) = kernel().await;
    for (name, word) in [("echo", "builtin"), ("greet", "function"), ("ll", "alias"), ("mytool", "file")] {
        let (out, _, code) = run(&kernel, &format!("type -t {name}")).await;
        assert_eq!((out.trim_end(), code), (word, 0), "{name}");
    }
}

#[tokio::test]
async fn type_of_a_missing_name_says_so_and_fails() {
    let (kernel, _dir) = kernel().await;
    let (out, err, code) = run(&kernel, "type nosuch").await;
    assert_eq!((out.as_str(), code), ("", 1));
    assert_eq!(err, "type: nosuch: not found\n");
}

#[tokio::test]
async fn command_without_v_is_refused_with_the_fix() {
    let (kernel, _dir) = kernel().await;
    let (out, err, code) = run(&kernel, "command echo hi").await;
    assert_eq!((out.as_str(), code), ("", 2), "{err}");
    assert!(err.contains("command -v echo"), "{err}");
}

#[tokio::test]
async fn kai_script_and_v_bin_path_resolve() {
    let (kernel, dir) = kernel().await;
    let setup = format!(
        "mkdir -p /scripts; echo 'echo hi' > /scripts/hello.kai; PATH=/scripts:{}",
        dir.path().display()
    );
    let (_, err, code) = run(&kernel, &setup).await;
    assert_eq!(code, 0, "{err}");
    let (out, _, code) = run(&kernel, "command -v hello").await;
    assert_eq!((out.trim_end(), code), ("/scripts/hello.kai", 0));
    let (out, _, _) = run(&kernel, "type hello").await;
    assert_eq!(out.trim_end(), "hello is /scripts/hello.kai");
    let (out, _, code) = run(&kernel, "command -v /v/bin/echo").await;
    assert_eq!((out.trim_end(), code), ("/v/bin/echo", 0));
    let (out, _, code) = run(&kernel, "command -v /v/bin/nosuch").await;
    assert_eq!((out.as_str(), code), ("", 1));
}

#[tokio::test]
async fn a_kernel_without_a_dispatcher_refuses_loudly() {
    let kernel = Kernel::new(KernelConfig::isolated()).expect("isolated kernel");
    let (_, err, code) = run(&kernel, "type echo").await;
    assert_eq!(code, 2, "{err}");
    assert!(err.contains("into_arc"), "{err}");
}
