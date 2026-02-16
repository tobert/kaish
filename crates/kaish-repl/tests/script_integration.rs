use std::path::Path;
use std::process::Command;

#[test]
fn integration_scripts() {
    let kaish = env!("CARGO_BIN_EXE_kaish");
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/../../tests/integration", manifest_dir);

    // Prepend the dev binary's directory to PATH so child `kaish` calls
    // resolve to the freshly-built binary, not a stale installed one.
    let bin_dir = Path::new(kaish).parent().expect("binary has parent dir");
    let path = std::env::var("PATH").unwrap_or_default();
    let new_path = format!("{}:{}", bin_dir.display(), path);

    let output = Command::new(kaish)
        .arg("runner.kai")
        .env("PATH", &new_path)
        .current_dir(&test_dir)
        .output()
        .expect("failed to run integration test runner");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stdout.is_empty() {
        println!("{}", stdout);
    }
    if !stderr.is_empty() {
        eprintln!("{}", stderr);
    }

    assert!(output.status.success(), "integration tests failed");
}
