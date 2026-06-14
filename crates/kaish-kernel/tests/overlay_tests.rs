//! Kernel-routed integration tests for overlay VFS mode (Part B).
//!
//! All tests that touch the real filesystem are gated behind
//! `#[cfg(feature = "localfs")]` (matching the project pattern from touch.rs
//! regression gate and the CLAUDE.md gate list).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

#[cfg(all(feature = "localfs", feature = "overlay"))]
mod overlay_tests {
    use kaish_kernel::{Kernel, KernelConfig};
    use std::path::Path;

    /// Build an overlay kernel rooted at `dir`.
    fn overlay_kernel(dir: &Path) -> Kernel {
        let config = KernelConfig::mcp_with_root(dir.to_path_buf())
            .with_overlay(true)
            .with_latch(false)
            .with_trash(false)
            .with_allow_external_commands(false);
        Kernel::new(config).expect("failed to create overlay kernel")
    }

    /// Run a script and return (stdout, exit_code).
    async fn run(kernel: &Kernel, script: &str) -> (String, i64) {
        let result = kernel.execute(script).await.expect("kernel execute");
        (result.text_out().trim().to_string(), result.code)
    }

    // ------------------------------------------------------------------
    // Helpers to extract labeled status fields from `kaish-vfs status`.
    //
    // The canonical table format is tab-separated: `key\tvalue` per line,
    // so `dirty\tyes` and `dirty\tno` are unambiguous even if the temp dir
    // path happens to contain the word "yes" or "no".
    // ------------------------------------------------------------------

    /// Return the VALUE cell for `key` from `kaish-vfs status` output.
    /// Panics with a descriptive message if the key is missing.
    fn status_field<'a>(status_out: &'a str, key: &str) -> &'a str {
        for line in status_out.lines() {
            // Each line is "key\tvalue"
            if let Some(rest) = line.strip_prefix(key) {
                if let Some(value) = rest.strip_prefix('\t') {
                    return value;
                }
            }
        }
        panic!("kaish-vfs status: field '{}' not found in:\n{}", key, status_out);
    }

    // ------------------------------------------------------------------
    // Test 1: overlay end-to-end (write, diff, commit, verify)
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn overlay_end_to_end_write_diff_commit() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();

        // Seed a file on the real disk.
        std::fs::write(root.join("hello.txt"), b"original content\n").unwrap();

        let kernel = overlay_kernel(root);
        let cwd = root.to_string_lossy();

        // Write through the overlay — real file should be unchanged.
        let (_, code) = run(&kernel, &format!(
            "echo 'edited content' > \"{}/hello.txt\"", cwd
        )).await;
        assert_eq!(code, 0, "overlay write should succeed");

        // Real file is still the original.
        let real_bytes = std::fs::read(root.join("hello.txt")).unwrap();
        assert_eq!(real_bytes, b"original content\n",
            "real file must be byte-identical after virtual write");

        // kaish-vfs status: should be dirty.
        let (status_out, code) = run(&kernel, "kaish-vfs status").await;
        assert_eq!(code, 0, "kaish-vfs status failed: {}", status_out);
        // Assert the labeled field, not a bare substring (finding #3: the temp
        // dir path can contain "yes" or "no").
        assert_eq!(status_field(&status_out, "dirty"), "yes",
            "overlay should be dirty: {}", status_out);

        // kaish-vfs diff: assert exact content-bearing +/- lines and headers.
        let display_path = format!("{}/hello.txt", cwd);
        let (diff_out, diff_code) = run(&kernel, "kaish-vfs diff").await;
        // diff exits 1 when there are changes (POSIX convention)
        assert_eq!(diff_code, 1, "diff should exit 1 when dirty");
        // Exact removed line (without leading -) must be present prefixed by '-'.
        assert!(diff_out.lines().any(|l| l == "-original content"),
            "diff output must contain '-original content' line: {}", diff_out);
        // Exact added line (without leading +) must be present prefixed by '+'.
        assert!(diff_out.lines().any(|l| l == "+edited content"),
            "diff output must contain '+edited content' line: {}", diff_out);
        // --- header must name the display path exactly once (regression: Added
        // files emitted the header twice before the double-header fix).
        let triple_minus_count = diff_out.lines()
            .filter(|l| l.starts_with("---") && l.contains(&*display_path))
            .count();
        assert_eq!(triple_minus_count, 1,
            "--- header for {} must appear exactly once: {}", display_path, diff_out);
        let triple_plus_count = diff_out.lines()
            .filter(|l| l.starts_with("+++") && l.contains(&*display_path))
            .count();
        assert_eq!(triple_plus_count, 1,
            "+++ header for {} must appear exactly once: {}", display_path, diff_out);

        // kaish-vfs commit: write changes to real files.
        let (commit_out, code) = run(&kernel, "kaish-vfs commit").await;
        assert_eq!(code, 0, "commit failed: {}", commit_out);

        // Real file now has the new content.
        let committed_bytes = std::fs::read(root.join("hello.txt")).unwrap();
        assert_eq!(committed_bytes, b"edited content\n",
            "real file should have new content after commit");

        // After commit, overlay should be clean.
        let (status_after, code) = run(&kernel, "kaish-vfs status").await;
        assert_eq!(code, 0);
        assert_eq!(status_field(&status_after, "dirty"), "no",
            "overlay should be clean after commit: {}", status_after);
    }

    // ------------------------------------------------------------------
    // Test 1b: commit of added, removed, and nested changes —
    // verifies the real tree before AND after commit.
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn overlay_commit_add_remove_nested() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();

        // Seed lower: a file to modify and a file to remove.
        std::fs::write(root.join("modify.txt"), b"before\n").unwrap();
        std::fs::write(root.join("to_remove.txt"), b"will be gone\n").unwrap();

        let kernel = overlay_kernel(root);
        let cwd = root.to_string_lossy();

        // ---- BEFORE COMMIT: real tree must be byte-identical ----

        // Modify modify.txt through overlay.
        let (_, code) = run(&kernel, &format!(
            "echo 'after' > \"{}/modify.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        // Add a new file in a NEW nested subdirectory (parent dirs created by commit).
        let (_, code) = run(&kernel, &format!(
            "mkdir -p \"{}/sub/dir\" && echo 'new nested' > \"{}/sub/dir/new.txt\"", cwd, cwd
        )).await;
        assert_eq!(code, 0, "mkdir+write in overlay should succeed");

        // Remove to_remove.txt through overlay.
        let (_, code) = run(&kernel, &format!(
            "rm \"{}/to_remove.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        // Real tree unchanged BEFORE commit.
        assert_eq!(std::fs::read(root.join("modify.txt")).unwrap(), b"before\n",
            "modify.txt must still have original bytes before commit");
        assert!(!root.join("sub").exists(),
            "sub/ dir must not exist on real disk before commit");
        assert!(root.join("to_remove.txt").exists(),
            "to_remove.txt must still exist on real disk before commit");

        // ---- COMMIT ----
        let (commit_out, code) = run(&kernel, "kaish-vfs commit").await;
        assert_eq!(code, 0, "commit failed: {}", commit_out);

        // ---- AFTER COMMIT: verify all three effects on real tree ----
        assert_eq!(std::fs::read(root.join("modify.txt")).unwrap(), b"after\n",
            "modify.txt must have new bytes after commit");
        let nested_bytes = std::fs::read(root.join("sub/dir/new.txt"))
            .expect("sub/dir/new.txt must exist after commit");
        assert_eq!(nested_bytes, b"new nested\n",
            "nested file must have exact bytes after commit");
        assert!(!root.join("to_remove.txt").exists(),
            "to_remove.txt must be GONE from real tree after commit");
    }

    // ------------------------------------------------------------------
    // Test 1c: diff for an Added file shows /dev/null header exactly once
    // (regression: double-header bug on Added files).
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn overlay_diff_added_file_header_appears_once() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();
        let kernel = overlay_kernel(root);
        let cwd = root.to_string_lossy();

        // Add a new file (no lower layer).
        let (_, code) = run(&kernel, &format!(
            "echo 'brand new' > \"{}/added.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        let (diff_out, diff_code) = run(&kernel, "kaish-vfs diff").await;
        assert_eq!(diff_code, 1, "diff should exit 1 when there are changes");

        // Added file: --- must be /dev/null, +++ must name the file.
        let display_path = format!("{}/added.txt", cwd);
        let minus_headers: Vec<&str> = diff_out.lines()
            .filter(|l| l.starts_with("---"))
            .collect();
        assert_eq!(minus_headers.len(), 1, "exactly one --- header: {:?}", minus_headers);
        assert!(minus_headers[0].contains("/dev/null"),
            "--- header for Added file must reference /dev/null: {}", minus_headers[0]);

        let plus_headers: Vec<&str> = diff_out.lines()
            .filter(|l| l.starts_with("+++"))
            .collect();
        assert_eq!(plus_headers.len(), 1, "exactly one +++ header: {:?}", plus_headers);
        assert!(plus_headers[0].contains(&*display_path),
            "+++ header must name the file path: {}", plus_headers[0]);

        // Content line must show the added text.
        assert!(diff_out.lines().any(|l| l == "+brand new"),
            "diff must contain '+brand new' line: {}", diff_out);
    }

    // ------------------------------------------------------------------
    // Test 2: reset discards virtual edits
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn overlay_reset_discards_edits() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();
        std::fs::write(root.join("data.txt"), b"original\n").unwrap();

        let kernel = overlay_kernel(root);
        let cwd = root.to_string_lossy();

        // Write through the overlay.
        let (_, code) = run(&kernel, &format!(
            "echo 'virtual' > \"{}/data.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        // Verify the overlay sees the new content.
        let (cat_out, code) = run(&kernel, &format!(
            "cat \"{}/data.txt\"", cwd
        )).await;
        assert_eq!(code, 0);
        assert!(cat_out.contains("virtual"), "overlay should see new content: {}", cat_out);

        // Reset all.
        let (reset_out, code) = run(&kernel, "kaish-vfs reset").await;
        assert_eq!(code, 0, "reset failed: {}", reset_out);

        // Overlay is clean — use labeled field assertion (finding #3).
        let (status, code) = run(&kernel, "kaish-vfs status").await;
        assert_eq!(code, 0);
        assert_eq!(status_field(&status, "dirty"), "no",
            "overlay should be clean after reset: {}", status);

        // Real tree is untouched.
        let real_bytes = std::fs::read(root.join("data.txt")).unwrap();
        assert_eq!(real_bytes, b"original\n", "real file should be untouched after reset");

        // VFS reads show the lower content again.
        let (cat_after, code) = run(&kernel, &format!(
            "cat \"{}/data.txt\"", cwd
        )).await;
        assert_eq!(code, 0);
        assert!(cat_after.contains("original"), "overlay should show lower content after reset: {}", cat_after);
    }

    // ------------------------------------------------------------------
    // Test 2b: single-path reset discards only that path's edit
    // (finding #5b)
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn overlay_reset_single_path_leaves_other_dirty() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();
        std::fs::write(root.join("file_a.txt"), b"aaa\n").unwrap();
        std::fs::write(root.join("file_b.txt"), b"bbb\n").unwrap();

        let kernel = overlay_kernel(root);
        let cwd = root.to_string_lossy();

        // Dirty both files.
        let (_, code) = run(&kernel, &format!(
            "echo 'modified_a' > \"{}/file_a.txt\"", cwd
        )).await;
        assert_eq!(code, 0);
        let (_, code) = run(&kernel, &format!(
            "echo 'modified_b' > \"{}/file_b.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        // Reset only file_a via relative path from cwd.
        let rel_a = "file_a.txt";
        let (reset_out, code) = run(&kernel, &format!(
            "cd \"{}\" && kaish-vfs reset {}", cwd, rel_a
        )).await;
        assert_eq!(code, 0, "single-path reset failed: {}", reset_out);

        // file_a reverts to original; file_b stays modified.
        let (cat_a, _) = run(&kernel, &format!("cat \"{}/file_a.txt\"", cwd)).await;
        assert!(cat_a.contains("aaa"), "file_a should revert: {}", cat_a);

        let (cat_b, _) = run(&kernel, &format!("cat \"{}/file_b.txt\"", cwd)).await;
        assert!(cat_b.contains("modified_b"), "file_b should still be modified: {}", cat_b);

        // Overlay must still be dirty (file_b untouched by reset).
        let (status, code) = run(&kernel, "kaish-vfs status").await;
        assert_eq!(code, 0);
        assert_eq!(status_field(&status, "mode"), "transaction");
        assert_eq!(status_field(&status, "dirty"), "yes",
            "overlay should still be dirty after partial reset: {}", status);
        assert_eq!(status_field(&status, "modified"), "1",
            "should have exactly 1 modified file remaining: {}", status);
    }

    // ------------------------------------------------------------------
    // Test 3: kaish-vfs on a non-overlay kernel — status reports direct
    // mode (introspection, no error path); transaction subcommands error.
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn kaish_vfs_without_overlay() {
        let kernel = Kernel::new(
            KernelConfig::isolated()
        ).expect("kernel");

        // status answers "what session am I in?" in any session.
        let result = kernel.execute("kaish-vfs status").await.expect("execute");
        assert_eq!(
            result.code, 0,
            "kaish-vfs status must work in any session: {}",
            result.err
        );
        let status = result.text_out();
        assert_eq!(status_field(&status, "mode"), "direct");
        assert_eq!(status_field(&status, "budget"), "unlimited");

        // The transaction subcommands stay loud without an overlay.
        for subcmd in ["diff", "commit", "reset"] {
            let result = kernel
                .execute(&format!("kaish-vfs {subcmd}"))
                .await
                .expect("execute");
            assert_ne!(
                result.code, 0,
                "kaish-vfs {subcmd} on a non-overlay kernel should exit non-zero"
            );
            let combined = format!("{}{}", result.text_out(), result.err);
            assert!(
                combined.contains("no overlay active"),
                "kaish-vfs {subcmd} should say 'no overlay active': {}",
                combined
            );
        }
    }

    // ------------------------------------------------------------------
    // Test 4: NoLocal + overlay=true fails at construction
    // ------------------------------------------------------------------

    #[test]
    fn nolocal_plus_overlay_fails_loudly() {
        let result = Kernel::new(
            KernelConfig::isolated().with_overlay(true)
        );
        assert!(result.is_err(), "NoLocal + overlay should fail at construction");
        let err_msg = result.map(|_| "ok".to_string()).unwrap_or_else(|e| e.to_string());
        assert!(err_msg.contains("NoLocal") || err_msg.contains("overlay") || err_msg.contains("virtual"),
            "error should mention NoLocal or overlay: {}", err_msg);
    }

    // ------------------------------------------------------------------
    // Test 5: Budget + overlay — writes that exceed budget fail in-band
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn overlay_budget_exceeded_fails_inband() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();

        // 100-byte budget; we'll try to write ~200 bytes.
        let config = KernelConfig::mcp_with_root(root.to_path_buf())
            .with_overlay(true)
            .with_vfs_budget(100)
            .with_latch(false)
            .with_trash(false)
            .with_allow_external_commands(false);
        let kernel = Kernel::new(config).expect("kernel");
        let cwd = root.to_string_lossy();

        // Write a lot of data (way more than 100 bytes)
        let big_script = format!(
            "echo 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' > \"{}/big.txt\"",
            cwd
        );
        let (out, code) = run(&kernel, &big_script).await;
        // Should fail with a budget error
        assert_ne!(code, 0, "write exceeding budget should fail: out={}", out);
        let err = kernel.execute(&big_script).await.expect("execute").err;
        assert!(err.contains("vfs-memory") || out.contains("vfs-memory"),
            "error should name 'vfs-memory' budget: err={:?}", err);
    }

    // ------------------------------------------------------------------
    // Test 6: glob over merged directory (doc test 12)
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn overlay_glob_over_merged_dir() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();

        // Lower has two files.
        std::fs::write(root.join("a.txt"), b"a").unwrap();
        std::fs::write(root.join("b.txt"), b"b").unwrap();

        let kernel = overlay_kernel(root);
        let cwd = root.to_string_lossy();

        // Add a new file via overlay.
        let (_, code) = run(&kernel, &format!(
            "echo 'c' > \"{}/c.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        // Remove b.txt via overlay (whiteout).
        let (_, code) = run(&kernel, &format!(
            "rm \"{}/b.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        // Glob should show a.txt + c.txt (union minus whiteout b.txt).
        let (glob_out, code) = run(&kernel, &format!(
            "for f in \"{}/\"*.txt; do echo $f; done", cwd
        )).await;
        assert_eq!(code, 0, "glob over merged dir failed: {}", glob_out);
        assert!(glob_out.contains("a.txt"), "a.txt should be in merged glob: {}", glob_out);
        assert!(glob_out.contains("c.txt"), "c.txt should be in merged glob: {}", glob_out);
        assert!(!glob_out.contains("b.txt"), "b.txt should be whiteouted: {}", glob_out);

        // Real directory must be untouched: c.txt absent, b.txt still present (finding #4).
        assert!(!root.join("c.txt").exists(),
            "c.txt must NOT exist on real disk (overlay-only file)");
        assert!(root.join("b.txt").exists(),
            "b.txt must still exist on real disk (only whiteouted in overlay)");
        assert!(root.join("a.txt").exists(),
            "a.txt must still exist on real disk (unchanged)");
    }

    // ------------------------------------------------------------------
    // Test 7: path-filtered diff filters to one file only (finding #5a).
    // Also locks in the relative-path resolution fix.
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn overlay_diff_path_filter_excludes_other_files() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();
        std::fs::write(root.join("alpha.txt"), b"alpha\n").unwrap();
        std::fs::write(root.join("beta.txt"), b"beta\n").unwrap();

        let kernel = overlay_kernel(root);
        let cwd = root.to_string_lossy();

        // Dirty both files.
        let (_, code) = run(&kernel, &format!(
            "echo 'ALPHA' > \"{}/alpha.txt\"", cwd
        )).await;
        assert_eq!(code, 0);
        let (_, code) = run(&kernel, &format!(
            "echo 'BETA' > \"{}/beta.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        // Filtered diff via relative path from cwd — only alpha.txt should appear.
        let (diff_out, diff_code) = run(&kernel, &format!(
            "cd \"{}\" && kaish-vfs diff alpha.txt", cwd
        )).await;
        assert_eq!(diff_code, 1, "filtered diff should exit 1 (changes present): {}", diff_out);
        assert!(diff_out.contains("alpha"), "filtered diff must mention alpha.txt: {}", diff_out);
        // beta's content must NOT appear.
        assert!(!diff_out.contains("beta") && !diff_out.contains("BETA"),
            "filtered diff must NOT include beta.txt changes: {}", diff_out);

        // Unfiltered diff shows both.
        let (full_diff, _) = run(&kernel, "kaish-vfs diff").await;
        assert!(full_diff.contains("alpha"), "unfiltered diff must show alpha: {}", full_diff);
        assert!(full_diff.contains("beta"), "unfiltered diff must show beta: {}", full_diff);
    }

    // ------------------------------------------------------------------
    // Test 8: commit conflict — conflicting file in real tree makes commit
    // fail in-band, overlay stays dirty (finding #5c).
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn overlay_commit_conflict_fails_inband() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();

        let kernel = overlay_kernel(root);
        let cwd = root.to_string_lossy();

        // Add a new file through the overlay (no lower layer).
        let (_, code) = run(&kernel, &format!(
            "echo 'overlay version' > \"{}/conflict.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        // Now create the same file directly in the real tempdir to cause a conflict.
        std::fs::write(root.join("conflict.txt"), b"real version\n").unwrap();

        // Commit must fail because the Added file now conflicts with an existing lower.
        let (commit_out, commit_code) = run(&kernel, "kaish-vfs commit").await;
        assert_ne!(commit_code, 0,
            "commit should fail when Added file conflicts with existing lower: {}", commit_out);
        let combined = format!("{}{}", commit_out,
            kernel.execute("kaish-vfs commit").await.map(|r| r.err).unwrap_or_default());
        assert!(
            combined.to_lowercase().contains("conflict") || commit_out.to_lowercase().contains("conflict"),
            "commit error must mention 'conflict': {}", commit_out
        );

        // Overlay must still be dirty after failed commit.
        let (status_after, status_code) = run(&kernel, "kaish-vfs status").await;
        assert_eq!(status_code, 0);
        assert_eq!(status_field(&status_after, "dirty"), "yes",
            "overlay must remain dirty after failed commit: {}", status_after);
    }

    // ------------------------------------------------------------------
    // Test 9: diff --json emits structured array (finding #7).
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn overlay_diff_json_is_structured() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();
        std::fs::write(root.join("src.txt"), b"old line\n").unwrap();

        let kernel = overlay_kernel(root);
        let cwd = root.to_string_lossy();

        // Modify a file.
        let (_, code) = run(&kernel, &format!(
            "echo 'new line' > \"{}/src.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        // Add a new file.
        let (_, code) = run(&kernel, &format!(
            "echo 'added' > \"{}/new.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        let result = kernel.execute("kaish-vfs diff --json").await.expect("execute");
        // diff exits 1 when changes exist (POSIX convention, even with --json).
        assert_eq!(result.code, 1, "kaish-vfs diff --json should exit 1 when dirty");

        let json_str = result.text_out();
        let v: serde_json::Value = serde_json::from_str(json_str.trim())
            .unwrap_or_else(|e| panic!("kaish-vfs diff --json must produce valid JSON: {e}\ngot: {json_str:?}"));

        let arr = v.as_array()
            .unwrap_or_else(|| panic!("kaish-vfs diff --json must be a JSON array, got: {v}"));
        assert!(!arr.is_empty(), "diff --json must not be empty when changes exist");

        // Each entry must have path, kind, base_bytes, current_bytes.
        for entry in arr {
            let obj = entry.as_object()
                .unwrap_or_else(|| panic!("each diff entry must be a JSON object: {entry}"));
            assert!(obj.contains_key("path"), "entry missing 'path': {entry}");
            assert!(obj.contains_key("kind"), "entry missing 'kind': {entry}");
            assert!(obj.contains_key("base_bytes"), "entry missing 'base_bytes': {entry}");
            assert!(obj.contains_key("current_bytes"), "entry missing 'current_bytes': {entry}");

            let kind = obj["kind"].as_str().expect("kind must be a string");
            assert!(
                kind == "added" || kind == "modified" || kind == "removed",
                "kind must be 'added', 'modified', or 'removed': {kind}"
            );
        }

        // Verify the modified entry has the right path.
        let src_entry = arr.iter().find(|e| {
            e["path"].as_str().map(|p| p.ends_with("src.txt")).unwrap_or(false)
        });
        assert!(src_entry.is_some(), "must have entry for src.txt: {arr:?}");
        assert_eq!(src_entry.unwrap()["kind"].as_str(), Some("modified"),
            "src.txt must be 'modified': {src_entry:?}");

        let new_entry = arr.iter().find(|e| {
            e["path"].as_str().map(|p| p.ends_with("new.txt")).unwrap_or(false)
        });
        assert!(new_entry.is_some(), "must have entry for new.txt: {arr:?}");
        assert_eq!(new_entry.unwrap()["kind"].as_str(), Some("added"),
            "new.txt must be 'added': {new_entry:?}");
    }

    // ------------------------------------------------------------------
    // Test 10: diff exits 0 when overlay is clean (finding #7).
    // ------------------------------------------------------------------

    #[tokio::test]
    async fn overlay_diff_exits_0_when_clean() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();
        let kernel = overlay_kernel(root);

        let (out, code) = run(&kernel, "kaish-vfs diff").await;
        assert_eq!(code, 0, "diff should exit 0 when overlay is clean: {}", out);
    }
}

// ------------------------------------------------------------------
// Test: diff headers for added/removed files (critic item 12)
// These use in-memory VFS so no localfs gate needed.
// ------------------------------------------------------------------

#[cfg(all(feature = "localfs", feature = "overlay"))]
mod diff_header_tests {
    use kaish_kernel::{Kernel, KernelConfig};

    async fn run(kernel: &Kernel, script: &str) -> (String, i64) {
        let result = kernel.execute(script).await.expect("kernel execute");
        (result.text_out().trim().to_string(), result.code)
    }

    #[tokio::test]
    async fn diff_added_file_shows_devnull_header() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path();

        let config = KernelConfig::mcp_with_root(root.to_path_buf())
            .with_overlay(true)
            .with_latch(false)
            .with_trash(false)
            .with_allow_external_commands(false);
        let kernel = Kernel::new(config).expect("kernel");
        let cwd = root.to_string_lossy();

        // Add a new file (no lower).
        let (_, code) = run(&kernel, &format!(
            "echo 'new file' > \"{}/new.txt\"", cwd
        )).await;
        assert_eq!(code, 0);

        let (diff_out, _) = run(&kernel, "kaish-vfs diff").await;
        // Should have /dev/null as the base header.
        assert!(diff_out.contains("/dev/null"),
            "added file diff should reference /dev/null: {}", diff_out);

        // --- header must appear exactly once (double-header regression check).
        let minus_headers: Vec<&str> = diff_out.lines()
            .filter(|l| l.starts_with("---"))
            .collect();
        assert_eq!(minus_headers.len(), 1,
            "exactly one --- header expected, got: {:?}", minus_headers);

        assert!(diff_out.lines().any(|l| l == "+new file"),
            "added file diff must contain '+new file' line: {}", diff_out);
    }
}
