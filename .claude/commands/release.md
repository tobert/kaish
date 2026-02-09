---
description: Tag and release kaish to crates.io. Runs full test suite, code review, version bump, commit, tag, push, and publish.
---

# Release kaish to crates.io

This command handles the full release workflow for the kaish workspace.
Follow each phase in order. Stop and report to the user if any phase fails.

$ARGUMENTS is the new version number (e.g. `0.2.0`). If not provided, ask the user.

## Phase 1: Pre-flight Checks

Verify the repo is ready for release:

1. Confirm on `main` branch with clean working tree (`git status`)
2. Confirm up to date with remote (`git fetch origin && git log HEAD..origin/main --oneline`)
3. Run `cargo clippy --all` — must be 0 warnings
4. Run `cargo test --all` — all must pass
5. Run `cargo insta test --check` — no pending snapshots
6. Check current version: `grep '^version' Cargo.toml` and compare with the requested version

If any check fails, stop and report. Do not proceed.

## Phase 2: Code Review

Before releasing, get a second opinion on changes since the last release tag.

1. Find the previous release tag: `git tag --sort=-v:refname | head -1`
2. Get the diff summary: `git diff <prev-tag>..HEAD --stat`
3. Review the changes using a frontier model:
   - If `consult_gemini_pro` is available (gpal MCP server), use it
   - Otherwise, use a Task subagent with `model: opus` for the review
   - Provide: `git log <prev-tag>..HEAD --oneline` and `git diff <prev-tag>..HEAD`
   - Ask for: breaking changes, API concerns, missing tests, documentation gaps
4. Report the review summary to the user
5. Ask the user to confirm proceeding with the release

## Phase 3: Version Bump

The workspace has a specific versioning structure. ALL of these must be updated to `$ARGUMENTS`:

### Workspace version (root Cargo.toml)
```
Cargo.toml → version = "$ARGUMENTS"
```

### Standalone crate versions (not using workspace version)
```
crates/kaish-glob/Cargo.toml   → version = "$ARGUMENTS"
crates/kaish-schema/Cargo.toml → version = "$ARGUMENTS"
```

### Inter-crate dependency versions
These use `path + version` for both local dev and crates.io publishing:
```
crates/kaish-kernel/Cargo.toml → kaish-glob version, kaish-schema version
crates/kaish-client/Cargo.toml → kaish-kernel version, kaish-schema version
crates/kaish-mcp/Cargo.toml    → kaish-kernel version
crates/kaish-repl/Cargo.toml   → kaish-kernel version, kaish-client version
```

After editing, run `cargo check --all` to verify the versions resolve correctly.

## Phase 4: Commit and Tag

1. Stage all modified Cargo.toml files by name (never `git add -A`)
2. Commit with message: `chore: bump to v$ARGUMENTS`
3. Create annotated tag: `git tag -a v$ARGUMENTS -m "Release v$ARGUMENTS"`
4. Run `git status` to verify clean tree

## Phase 5: Push

1. Ask the user to confirm pushing to origin
2. Push commit and tag: `git push origin main && git push origin v$ARGUMENTS`

## Phase 6: Publish to crates.io

Publish in dependency order. Each crate must be available on crates.io before
its dependents can be published. Wait 15 seconds between publishes.

```
cargo publish -p kaish-glob
# wait 15s
cargo publish -p kaish-schema
# wait 15s
cargo publish -p kaish-kernel
# wait 15s
cargo publish -p kaish-client
# wait 15s — client and mcp are independent, but serial is safer
cargo publish -p kaish-mcp
# wait 15s
cargo publish -p kaish-repl
```

If a publish fails with "crate version already exists", that crate was
already published — skip it and continue with the next one.

If a publish fails because a dependency isn't available yet, wait 30 seconds
and retry once.

## Phase 7: Verify

1. Check the release is visible: `cargo search kaish-repl`
2. Report the published versions to the user
3. Optionally create a GitHub release: `gh release create v$ARGUMENTS --generate-notes`

## Known Issues

- `kaish-glob` and `kaish-schema` use standalone `version = "X.Y.Z"` instead
  of `version.workspace = true`. Both must be bumped manually.
- All inter-crate deps pin exact versions. These must match what's being published.
- crates.io has a propagation delay — the 15s waits handle this.
