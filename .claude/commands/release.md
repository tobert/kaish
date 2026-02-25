---
description: Tag and release kaish to crates.io. Runs full test suite, code review, version bump, commit, tag, push, and publish.
---

# Release kaish to crates.io

This command handles the full release workflow for the kaish workspace.
Follow each phase in order. Stop and report to the user if any phase fails.

$ARGUMENTS is the new version number (e.g. `0.2.0`). If not provided, ask the user.

## Phase 0: Version Sanity Check

Before doing any work, parse the current version from `Cargo.toml` and compare
with the requested version. Flag and confirm with the user if:

- **Major bump** (e.g. `0.x.y` → `1.0.0`) — "This is a major version bump. Intentional?"
- **Minor skip** (e.g. `0.1.4` → `0.5.0`) — "This skips minor versions 0.2–0.4. Intentional?"
- **Downgrade** — "The requested version is lower than the current version. Typo?"
- **Same version** — "This version is already set. Nothing to release."

Normal increments (patch bump like `0.1.4` → `0.1.5`, or minor bump like
`0.1.4` → `0.2.0`) proceed without asking.

## Phase 1: Pre-flight Checks

Verify the repo is ready for release:

1. Confirm on `main` branch with clean working tree (`git status`)
2. Confirm up to date with remote (`git fetch origin && git log HEAD..origin/main --oneline`)
3. Run `cargo clippy --all` — must be 0 warnings
4. Run `cargo test --all` — all must pass
5. Run `cargo insta test --check` — no pending snapshots

If any check fails, stop and report. Do not proceed.

## Phase 2: Docs Consistency Check

If any builtins were added, removed, or recategorized since the last tag,
verify that documentation matches the actual tool registry. Launch a Task
subagent (Explore type) to:

1. List every builtin tool name from `crates/kaish-kernel/src/tools/builtin/`
   (each file's `fn name(&self)` return value)
2. Compare against the builtin table in `README.md` (the `| Category | Tools |` table)
3. Compare against the category match arms in `crates/kaish-kernel/src/help.rs`
   (`format_tool_list` function)
4. Check for hardcoded tool counts anywhere in docs (`README.md`, `docs/help/*.md`)

Report any mismatches: ghost entries (listed but don't exist), missing entries
(exist but not listed), wrong categories, stale counts. Fix them before proceeding.

If no builtins changed since the last tag, skip this phase.

## Phase 3: Code Review

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

## Phase 4: Version Bump

The workspace has a specific versioning structure. ALL of these must be updated to `$ARGUMENTS`:

### Workspace version (root Cargo.toml)
```
Cargo.toml → version = "$ARGUMENTS"
```

### Standalone crate versions (not using workspace version)
```
crates/kaish-glob/Cargo.toml   → version = "$ARGUMENTS"
```

### Inter-crate dependency versions
These use `path + version` for both local dev and crates.io publishing:
```
crates/kaish-kernel/Cargo.toml → kaish-glob version, kaish-types version
crates/kaish-client/Cargo.toml → kaish-kernel version
crates/kaish-mcp/Cargo.toml    → kaish-kernel version
crates/kaish-repl/Cargo.toml   → kaish-kernel version, kaish-client version
```

After editing, run `cargo check --all` to verify the versions resolve correctly.

## Phase 5: Commit and Tag

1. Stage all modified Cargo.toml files by name (never `git add -A`)
2. Commit with message: `chore: bump to v$ARGUMENTS`
3. Create annotated tag: `git tag -a v$ARGUMENTS -m "Release v$ARGUMENTS"`
4. Run `git status` to verify clean tree

## Phase 6: Push

1. Ask the user to confirm pushing to origin
2. Push commit and tag: `git push origin main && git push origin v$ARGUMENTS`

## Phase 7: Publish to crates.io

Publish in dependency order. Each crate must be available on crates.io before
its dependents can be published. Wait 15 seconds between publishes.

```
cargo publish -p kaish-glob
# wait 15s
cargo publish -p kaish-types
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

## Phase 8: Verify

1. Check the release is visible: `cargo search kaish-repl`
2. Report the published versions to the user
3. Optionally create a GitHub release: `gh release create v$ARGUMENTS --generate-notes`

## Known Issues

- `kaish-glob` uses standalone `version = "X.Y.Z"` instead
  of `version.workspace = true` and must be bumped manually.
- All inter-crate deps pin exact versions. These must match what's being published.
- crates.io has a propagation delay — the 15s waits handle this.
