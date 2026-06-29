---
description: Tag and release kaish to crates.io. Runs full test suite, code review, version bump on a release branch, merges the bump via PR, then tags from main and publishes.
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

## Phase 3: Changelog Consistency Check

The `## [Unreleased]` section of `CHANGELOG.md` must account for every
user/agent/embedder-facing change since the last tag — not just the ones whose
commits happened to touch the changelog. Reconcile it against git before
releasing:

1. List commits since the last tag (oldest first):
   `git log <prev-tag>..HEAD --oneline --reverse`
2. For each commit that changes observable behavior, confirm an `Unreleased`
   bullet exists in the right group (`Added`/`Changed`/`Deprecated`/`Removed`/
   `Fixed`/`Security`). Read the commit diff (`git show <sha>`) when the message
   alone doesn't make the user-facing effect clear.
3. Skip pure internal churn — `docs(issues)` punch-list edits, `test:`-only
   commits, `chore(clippy)`/lint cleanups, refactors with no observable effect,
   and version bumps. Everything else needs a bullet.
4. Write any missing bullets: one concise scannable line each, mark breaking
   changes with a leading `**BREAKING:**`, and verify the behavior from the diff
   rather than paraphrasing the commit subject.

Note: a commit's own diff touching `CHANGELOG.md` is **not** proof of coverage —
a feature batch is often documented under the version it shipped in, while
later fixes to it land post-tag and are easy to miss. Trust the commit list,
not the changelog's git history.

Report what you added, then continue. (The release-time rename of `Unreleased`
to the dated version section happens in Phase 5.)

## Phase 4: Code Review

Before releasing, get a second opinion on changes since the last release tag.

1. Find the previous release tag: `git tag --sort=-v:refname | head -1`
2. Get the diff summary: `git diff <prev-tag>..HEAD --stat`
3. Review the changes with **kaibo** (`consult`, `cast=deepseek` — our default
   review cast). Point it at the repo and the release surface in prose; let it
   read the current code itself rather than pasting a diff. Ask for: breaking
   changes, API concerns, missing tests, documentation gaps. (If kaibo is
   unavailable, fall back to a Task subagent with `model: opus`.)
4. Report the review summary to the user
5. Ask the user to confirm proceeding with the release

## Phase 5: Version Bump (on a release branch)

**`main` is protected, so the version bump goes through a PR like every other
change** — it cannot be pushed to `main` directly. Only the tag and the publish
(Phases 7–8) run from `main` (neither is a branch commit), after the bump PR has
merged.

Create the release branch off the current (clean, up-to-date) `main`:

```
git switch -c release/v$ARGUMENTS
```

Do all of the edits below on this branch.

All crates use `version.workspace = true`, so the crate versions come from one
place — but every `path + version` inter-crate dependency pin must be bumped
to match. ALL of these must be updated to `$ARGUMENTS`:

### Workspace version (root Cargo.toml)
```
Cargo.toml → version = "$ARGUMENTS"
```

### Inter-crate dependency version pins
These use `path + version` for both local dev and crates.io publishing.
Find them all with:
```
grep -n 'kaish-.*version = "' crates/*/Cargo.toml
```
As of 0.8.x that is:
```
crates/kaish-tool-api/Cargo.toml   → kaish-types
crates/kaish-glob/Cargo.toml       → (no kaish deps)
crates/kaish-vfs/Cargo.toml        → kaish-types
crates/kaish-help/Cargo.toml       → kaish-types
crates/kaish-tools-host/Cargo.toml → kaish-types, kaish-tool-api
crates/kaish-kernel/Cargo.toml     → kaish-glob, kaish-types, kaish-help,
                                     kaish-tool-api, kaish-vfs,
                                     kaish-tools-host (optional dep — still pinned)
crates/kaish-client/Cargo.toml     → kaish-kernel
crates/kaish-repl/Cargo.toml       → kaish-kernel, kaish-client
```

After editing, run `cargo check --all` to verify the versions resolve correctly.

### Stamp the changelog

Now rename the changelog's `Unreleased` section to this release:

1. In `CHANGELOG.md`, rename `## [Unreleased]` to `## [$ARGUMENTS] - YYYY-MM-DD`
   (today's date).
2. Add a fresh empty `## [Unreleased]` heading above it.
3. Add the `[$ARGUMENTS]` compare link at the bottom of the file, following the
   existing pattern (`[$ARGUMENTS]: https://…/compare/v<prev>...v$ARGUMENTS`),
   and update the `[Unreleased]` compare link to point at `v$ARGUMENTS...HEAD`.

## Phase 6: Commit and Open the Release PR

The bump lands on `main` via a reviewed PR — **do not tag yet** (the tag must
point at the commit that actually lands on `main`, which may be a squash commit).

1. Stage all modified Cargo.toml files **and `CHANGELOG.md`** by name (never `git add -A`)
2. Commit on the release branch with message: `chore: bump to v$ARGUMENTS`
3. Push the branch: `git push -u origin release/v$ARGUMENTS`
4. Open the PR: `gh pr create --base main --title "chore: bump to v$ARGUMENTS" --body …`
   — the body summarizes that this is the version bump + changelog stamp for the
   release, and links the Phase 4 review.
5. **Have the PR reviewed before merging** (`/code-review` on the diff, or another
   agent/model). The bump diff itself is mechanical — version strings + the
   changelog stamp — so this is a light check that the pins all moved together and
   the changelog stamped cleanly; the substantive review of the release *contents*
   already happened in Phase 4.
6. Ask the user to confirm merging, then merge to `main`
   (`gh pr merge --squash --delete-branch` unless the user prefers a merge commit).

## Phase 7: Tag and Push from main

Now that the bump is on `main`, tag it there and push the tag:

1. `git switch main && git pull origin main` — fast-forward to the merged bump
2. Confirm `HEAD` carries the bump: `grep '^version' Cargo.toml` shows `$ARGUMENTS`,
   and `git log -1 --oneline` is the bump commit
3. Create the annotated tag at this commit:
   `git tag -a v$ARGUMENTS -m "Release v$ARGUMENTS"`
4. Ask the user to confirm pushing the tag, then: `git push origin v$ARGUMENTS`
   (the commit is already on `origin/main` from the merge — only the tag is pushed)

## Phase 8: Publish to crates.io

Publish in dependency order. Each crate must be available on crates.io before
its dependents can be published. Wait 15 seconds between publishes.
`kaish-wasi` is NOT published (wasm binary target, no library consumers).

```
cargo publish -p kaish-types
# wait 15s — tier 2 depends only on kaish-types (kaish-glob on nothing)
cargo publish -p kaish-glob
cargo publish -p kaish-tool-api
cargo publish -p kaish-vfs
cargo publish -p kaish-help
# wait 15s — the host tool bundle needs types/tool-api
cargo publish -p kaish-tools-host
# wait 15s — the kernel needs everything above (incl. its optional deps)
cargo publish -p kaish-kernel
# wait 15s
cargo publish -p kaish-client
# wait 15s
cargo publish -p kaish-repl
```

If a publish fails with "crate version already exists", that crate was
already published — skip it and continue with the next one.

If a publish fails because a dependency isn't available yet, wait 30 seconds
and retry once.

## Phase 9: Verify

1. Check the release is visible: `cargo search kaish-repl`
2. Report the published versions to the user
3. Optionally create a GitHub release: `gh release create v$ARGUMENTS --generate-notes`

## Known Issues

- All crates use `version.workspace = true`, so Phase 5 is just the root
  workspace version plus the inter-crate `path + version` pins (20 strings as of
  0.8.3). `kaish-glob` is no longer special-cased.
- All inter-crate deps pin exact versions. These must match what's being published.
- crates.io has a propagation delay — `cargo publish` blocks until the crate is
  available at the registry before returning, which covers it (the 15s waits are
  belt-and-suspenders; foreground `sleep` is blocked in some harnesses anyway).
- The crates.io token needs the **`publish-new`** scope for first-time crate
  publishes; a publish-update-only token 403s on a brand-new crate.
- `kaish-wasi` is deliberately **not published** (wasm binary target, no library
  consumers) — it is absent from the Phase 8 publish list on purpose.
