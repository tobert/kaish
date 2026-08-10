# kaish devlog

Narrative history of landed work and the decisions behind it — the "how we got
here" color that used to clutter the old issues.md backlog (verified and migrated
to GitHub Issues #175–#203, 2026-07-16) and tax every read of it.
This is *not* the authoritative record: `CHANGELOG.md` (per-version, user-facing)
and `git log` (commits, SHAs) are canonical. This is the story.

Newest themes first within each area; dates are when the work landed.

Write entries **late** — just before signoff or opening a PR — so they carry with
the PR and describe the work as it actually landed. Not early, not mid-flight: the
decisions aren't settled yet, and an entry written ahead of its change goes stale
before it ships.

---

## The eighth state gets a name (2026-08-10)

The previous entry ended by naming two questions and deliberately not
answering them: whether `chain.closed_by_settlement` becomes a `RequestState`
variant or gets blessed as a documented flag, and whether `TokenRejected`'s
revision bump earns a documenting line. Amy ruled on both. Both toward the
same answer: make it explicit.

The first one was the real work. The request state machine had eight states
and the enum had seven. A chain whose attempt settled `Exit(0)` or `Unknown`
stayed `RequestState::Granted` while a private bool flipped true, and five
sites in `core.rs` special-cased `Granted && closed_by_settlement` to behave
as terminal. The spec's B.3 table wrote that state as lowercase "closed" —
lowercase because the type had no name to capitalize. And an embedder reading
a serialized record could not see closure in the `state` field at all; it had
to walk the entry log for a `Settled` and decide for itself whether the
outcome was one of the two that close.

Choosing the name was most of the thinking, and three obvious candidates were
all wrong for reasons worth writing down. Not `Settled`: that is already the
entry name, and the entry fires for outcomes that do *not* close — a non-zero
exit leaves the grant live for a retry — so a state spelled `Settled` would
be true of chains that are still open. Not `Closed`: `mark_closed` applies to
all six terminal states, so the word already carries the broader concept and
a variant would collide with it. Not `Completed` or `Done` or `Fulfilled`:
the Terms table shipped "'The operation ran' is a fact about an attempt,
never about a request" two days ago, and `Outcome::Unknown` reaches this state
in exactly the case where nobody knows whether it ran.

`Consumed` came from the spec's own vocabulary. A.1 and the `grant` row of the
Terms table both say a failed attempt "does not consume" the grant; the
positive form of that sentence names the state a successful one produces. It
names the grant, not the work, which is precisely the distinction the Terms
table asks for.

Two things fell out of the change that the flag had been hiding. The expiry
skip became structural: `materialize_expiry` used to test
`Granted && !closed_by_settlement` before looking at `not_after`, and now it
simply never sees a consumed chain as `Granted`, so an `Expired` entry can no
longer be appended after the operation ran. And `settle()` needs a guard the
flag did not: a chain that closed some *other* way — voided by a fifth bad
key, expired past `not_after` — while its attempt was still `Reserved` must
keep the state that closed it. Setting `Consumed` unconditionally would erase
the `void_reason` the terminal error reports. With a bool there was no state
to overwrite, so the hazard did not exist; promoting it to a variant created
the hazard and the fix in the same edit.

One behavior deliberately changed, because the ruling said so: `grant`,
`deny`, `cancel`, and `abandon` against a consumed chain used to answer
`AlreadyDecided` (the chain was nominally `Granted`, after all) and now answer
`Terminal` naming `Consumed`. The B.4 nuance survives untouched and is the one
exception: redemption still reports the settled outcome and re-executes
nothing. That arm moved out of a post-match flag check and into the state
match itself, at both redeem sites, which is where a reader looks for it.

The second ruling was small and is mostly a paragraph. `bumps_revision` names
four entries that are not transitions of their request and explains each;
`TokenRejected` bumped too, but only by falling out of the wildcard arm.
Reading the code you could not tell whether that was a decision. It is: a
rejected key advances the count toward the fifth that voids the request, so
the risk an approver read is not the risk in front of it any more, and being
forced to re-read is the safe direction. Named arm, rustdoc line, and a test
that walks the sequence the contract exists for — read, bad key, stale answer
refused, fresh read, grant.

## The consolidation pass, and the five places the spec had drifted (2026-08-10)

PR 9 was supposed to be the tidy-up: fold §H's remaining-work lanes into the
landed table now that R5, the REPL, and the `PendingApproval` widening have all
shipped, sweep the future tense, refresh the Terms tables, done. It was that.
It was also the first time anybody read the whole 3,000-line spec end to end
against the code that had grown under it, and reading it that way is a
different activity from reviewing a lane's diff. A lane review asks "does this
diff do what its PR body says?" Eleven of those passed. What none of them could
ask is "does the spec still describe the thing we built?"

Five places it did not. None of them is a bug — every one is the spec having
been written before the code and never corrected afterwards, which is the
failure mode a living design doc has instead of bit rot.

**`LedgerSink::post` takes a `LedgerRecord`.** §D.4 said `&LedgerEntry`. The
envelope — `schema_version`, `sequence`, `scope` — is the whole reason an
external reader can tell whose record it is holding and whether it understands
it, and the sink is the one consumer that is definitionally external. The spec
had the envelope right in §A.5 and the sink signature wrong two sections later.

**`Subscribed` and `Unsubscribed` are entries and the spec had neither.** Not
in §A.1's posting table, not in §A.5's enum. The code comment on
`ApproverHandle::subscribe` gets the reasoning exactly right — "an audit scope
that changed with no record of the change would make the record it produced
unreadable" — and the spec never learned it.

**`with_ledger` takes a `LedgerConfig`.** §D.2 said `.with_ledger(Ledger)  //
share one ledger across kernels in this process`, which is two errors in one
line: the argument is sizing, and sharing is `with_approver_handle`, which
adopts the handle's ledger. `Kernel::build` returns a `Result`, not a bare
tuple. And `with_own_authority` and `with_approval_clock` — both real, both
load-bearing for the REPL and for any embedder that wants a testable clock —
were absent from the embedder API section entirely.

**There is no `ledger.live_requests` metric.** §D.4 and §I.1 both leaned on it:
"per-principal quotas and a `ledger.live_requests` metric make the DoS case
visible before it becomes an outage," and "the metric exists so the answer is
measurable rather than argued." It does not exist. The counts are
`LedgerState::live_count_total` and `live_count_by_principal`, both private,
neither exported. This one is the one that stings, because §A.10's whole
argument for deleting expiry rests on the failure being *visible*: "the failure
mode is *the ledger is full*, with a number, at a point where someone can act."
The number is real — `LedgerError::LiveCapacity` names the limit — but nothing
lets you watch the count approach it. Both sections now say the live count is
not exported and that walking `Approvals::pending(..)` is how you watch it.
Exporting a gauge is unclaimed work; it rides in the PR body rather than an
issue, because a merged PR is a sufficient record.

**§0 said two spellings of the retired word survive. Neither does.** That
paragraph was written before §I.4 was resolved and named the shell option and
`JobStatus::Latched` as the survivors; the cutover renamed both. What actually
survives is worse and the spec did not mention it: `set` ignores an unknown
`-o` name and exits **0**, for bash compatibility, so `set -o latch` turns
nothing on and says nothing about it. Somebody typing it wants a gate and gets
silence. That is the exact shape of failure kaish refuses everywhere else, and
it is sitting in a `_ => {}` arm in `set.rs` under the comment "silently ignore
for bash compatibility." We documented it in three places rather than changing
it — a `set` that rejects unknown options is a behavior change with its own
blast radius and does not belong in a docs PR — but it should be said out loud
that documenting a silent no-op is a consolation prize, not a fix.

A sixth thing turned up that was never in the spec at all: `rm` never trashes a
symlink. `decide_rm_action` unlinks the link and, under `approvals`, gates it —
because trashing the link would move its *target* to Trash, and the link is
trivially recreatable while the target may not be. So "trash wins over
approvals," which LANGUAGE.md and the help fragment both stated flatly, has an
exception precisely where following the general rule would delete the wrong
thing. That branch has been in `rm.rs` since the trash rail shipped and no
reader-facing text mentioned it.

The Terms table produced the one genuinely interesting decision. Retiring
`latch` and `nonce` is easy: the mechanism is gone, and a term with no referent
teaches a wrong model. But `latch` is *in active use in the corpus* in a
different sense — the ledger latches its view of the installed clock, holding
the largest reading it has taken so an expired grant stays expired. Two senses
of one word is exactly the collision the table exists to catch, and the
temptation was to add a second row. We did not: the clock latch is a mechanism
an embedder does not need in order to predict behavior (the guarantee is
"monotone non-decreasing", and that is what `EMBEDDING.md` promises), so it
carries no guarantee to publish. §0 says the collision out loud instead, next
to the retirement, so the next person who greps for `latch` and finds forty
hits in `core.rs` knows immediately which sense they are in.

Two questions stayed deliberately unanswered, both Amy's: whether
`chain.closed_by_settlement` becomes a `RequestState` variant or gets blessed
as a documented flag, and whether `TokenRejected`'s revision-bump earns a
documenting line. §B.3's lowercase "closed" and the silence around
`bumps_revision`'s wildcard arm are load-bearing neutrality, not oversights.
Consolidation touched sentences on both sides of each and left the wording
exactly as it stood.

## ExecResult carries the resume route it was built with (2026-08-10)

The previous entry's own verdict named the fix before this branch existed: "the
pending decision on `ExecResult` should carry the `ResumeAction` it was built
with, rather than making every frontend re-derive it." `fulfill_gate` was the
proof that mattered — the REPL, a plain embedder with no privileged hook, had
to call `ResumeAction::for_capture(&view.capture, &view.binding.plan_digest)`
itself to get the route `proceed()` had already computed and thrown away at the
result boundary.

The fix is the field carrying what the constructor actually builds:
`ExecResult.approval` is now `Option<Box<PendingApproval>>`, not
`Option<Box<ApprovalRequestView>>`. `proceed()` stores the whole
`PendingApproval` it builds instead of unpacking `.request` and dropping
`.resume`; `fulfill_gate` reads `gated.pending_approval()` and is done — no
`for_capture` call left in a consumer.

The interesting part was where `PendingApproval`/`ResumeAction` had to live.
They were defined in `kaish-tool-api`, which depends on `kaish-types` (where
`ExecResult` lives) and not the other way — so `ExecResult` could not name
them without an illegal back-edge. They moved down into `kaish-types`, next to
`ApprovalRequestView`, `Capture`, and everything else they are built from;
`kaish-tool-api` re-exports both names so no import at any call site had to
change. One accidental win: `ResumeAction::for_capture`'s match over `Capture`
had carried a defensive wildcard arm for `Capture`'s `#[non_exhaustive]`
marker since it lived in a foreign crate. Inside `kaish-types`, `Capture` is a
local type and the match is already exhaustive — the wildcard is now
unreachable and dropped, so a future `Capture` variant fails this match at
compile time instead of silently falling through it.

Two boundaries got a second look and were left alone on purpose. `ToolResult`
(the `KernelBackend` FFI-shaped result) keeps `approval: Option<Box<ApprovalRequestView>>`
— a backend tool crossing that boundary has no in-process `Kernel::confirm` to
resume through, so the route has nothing to mean there; the `ExecResult ↔
ToolResult` conversions project to the view on the way out and re-derive the
route with `PendingApproval::new` (a pure function of the view's own
`capture`/`binding`) on the way back in — exact, not a guess. And the `--json`
envelope's `"approval"` key stays the flat view it always was: `docs/approval-ledger.md`
§A.2 names `--json` as one of the surfaces that sees the tokenless view, and
the resume route is a Rust-level convenience for a caller driving the kernel
in-process, not part of that wire contract. Both boundaries got a pinning
test so the shape stays a decision, not a drift.

Ledger-era surface, never released — no `BREAKING` changelog marker for a
field type nobody outside this repo has depended on yet.

---

## The REPL fulfils its own gates, and the design survives its first real consumer (2026-08-10)

PR 11 of the approval ledger was written as the design's own proof: the REPL is a
plain embedder with no privileged hook, so if a human at a prompt cannot be served
by `Pending` + `grant` + `confirm`, §C.2 is wrong and this is where it shows.

It is not wrong. The whole prompt flow is about seventy lines above the kernel —
`fulfill_gate` reads `ExecResult.approval`, renders it, asks, and then either
`grant_with_grounds` + `Kernel::confirm` or `deny`. Nothing in the kernel is held
open while the human reads; the REPL's wait is a `readline`, which is the right
and only bound. §C.3 predicted "about fifteen lines"; the extra fifty are
rendering, the standing-grant path for `a`, and telling the truth about what did
not run.

Three things the constraint made awkward, all of them worth the trip:

**A single-kernel embedder could not hold its own authority.** `with_approver_handle`
installs an authority *and* adopts that handle's ledger, so it needs a handle,
which needs an earlier kernel. The REPL has none. The workaround available with
today's API is to build a throwaway kernel purely to mint a handle and then throw
it away — which puts a kernel id in the record that names no session anyone can
point at. `KernelConfig::with_own_authority(bool)` says the thing directly:
`Kernel::build` returns the handle *and* leaves a clone on the session. The
default stays `false`, because the default *is* the enforcement.

**Ctrl-C at the prompt had to be a keystroke, not a signal.** §C.3 says so and
the reason is now concrete: after the first `execute`, tokio owns SIGINT
process-wide with `SA_RESTART`, so a plain `read` on the terminal would neither
be interrupted nor killed — Ctrl-C would do *nothing* until the user pressed
Enter. Reading the answer through rustyline, which holds the terminal in raw
mode, turns `^C` into an ordinary `Interrupted` return. The PTY test spawns kaish
with ISIG genuinely on, so it fails if that ever stops being true.

**The prompt cannot be output.** The request renders to stderr, and the question
is only written when stdin *and* stdout are both terminals — the check lives
beside the write rather than at a distant construction site. `cli_approval_tests`
proves the negative the spec asks for: with every stream a pipe, neither `grant?`
nor `[y/a/N]` appears anywhere a caller could collect it, and the line exits 2.

The reference classifier question answered itself: R4's `CommandNameClassifier`
already matches the parsed plan's argv0, and a regex over the rendered line would
be strictly worse — it re-introduces the `echo 'rm -rf /'` false positive the plan
exists to remove. So the REPL *ships* the one that exists rather than adding a
second: `kaish --gate rm,kaish-trash`.

### ResumeAction: what the first consumer actually needed

The pre-tag question was whether `ConfirmStatement { plan_digest, index }`
suffices without packaged source text. Two findings, and they point in different
directions.

`index` was never used to *resume*, because it cannot be. `Kernel::confirm`
re-parses the captured source and runs statement `index` itself, so the REPL
hands over an id and gets a result — the happy path never touches `index` at all.
What the REPL wanted `index` for was the opposite: naming what it would *not* run.
`echo one; touch b` with `echo` gated runs statement 0 on approval and never
touches statement 1, and a line that half-runs in silence is exactly the failure
mode this design keeps refusing elsewhere. Counting the remainder took a re-parse
(`parser::parse(line)` and skip `index + 1`) — cheap, but shell-grammar work an
embedder holding an opaque program string should not have to do. Re-*running* the
remainder is not available at all: statements carry no source spans, so slicing
the tail back out of the line needs an unparser nobody has.

The sharper finding is one level up. `ExecResult.approval` carries the
`ApprovalRequestView` alone: `proceed()` builds a `PendingApproval`, puts the view
on the field, and drops the `ResumeAction`. So the frontend §C.1 wrote that field
for has to rebuild the route with `ResumeAction::for_capture(&view.capture,
&view.binding.plan_digest)` — inferring it from the capture's shape, which is the
one thing the type was introduced to prevent. The recipe in `EMBEDDING.md` said
otherwise and has been corrected to what a consumer must actually write.

Verdict: **document as is; do not package source text into the variant.** The
kernel already holds the source in `Capture::Statement` and replays from it, so a
copy in `ResumeAction` would be a second spelling of the same bytes, and the
embedder that would use it — one re-driving a remainder — needs a *rendering* of
statements `index + 1 ..`, which no amount of raw source gives it without a
parser. If that use case ever earns first-class support, the right shape is a
kernel-side "continue this program from index N" call, not a text field on a
resume hint. What should change before the tag is smaller and independent: the
pending decision on `ExecResult` should carry the `ResumeAction` it was built
with, rather than making every frontend re-derive it.

---

## Revision checks: making a race report itself, not whatever it landed on (2026-08-09)

§B.6 reads as one sentence — a stale-revision decision is refused and recorded —
but the ordering question underneath it took longer to settle than the code did.
`grant`, `deny`, and `cancel` each already had a state-machine check: `Requested`
proceeds, `Granted` is `AlreadyDecided`, anything terminal is `Terminal`. The
question was where the revision check goes relative to that check, and the
acceptance test that mattered — "a cancel racing a grant leaves exactly one winner
and exactly one `RevisionRejected`" — only passes one way. Put the revision check
*after* the state check and the loser sees whatever state the winner left behind
(`AlreadyDecided` for a cancel that lost to a grant); put it *before*, and the
loser is told the truth: its view was stale. The transition table in §B.3 already
implied the ordering — the "stale revision" rows sit next to `Requested`/`Granted`
rather than folded into `AlreadyDecided`'s row — but seeing it work against a real
concurrent test was what made the choice feel inevitable rather than arbitrary.

That ordering also resolves something that looked like a contradiction at first
read: does a current-but-illegal transition (grant an already-granted request,
quoting the right revision) still get `AlreadyDecided`, or does revision-checking
swallow it too? It still gets `AlreadyDecided` — because if the quote matches
current, by construction nothing has moved since the caller last looked, so the
state-machine check runs exactly as before. The two checks answer different
questions and only one of them can fire on any given call: "is your view
current?" and "is this transition legal?"

**Three decisions the task handed me rather than the spec.** The CLI has no
revision to remember between commands — a human typing `approvals grant req_123`
isn't carrying a number in their head — so `cmd_grant`/`cmd_deny`/`cmd_cancel`
each read the request's chain immediately before acting and quote whatever
revision that read saw. `cmd_grant` already did this read to build `GrantTerms`
from the request's declared transitions; the revision just rides along on the
same snapshot. Teardown was the one I expected to want an exemption and didn't
give one: `cancel_job_request`/`cancel_scope` quote whatever revision their own
read saw (a job's cached `PendingApproval`, or a fresh `pending()` scan) rather
than forcing the cancel through unconditionally. A `StaleRevision` there means a
human or a standing rule decided the request in the window between teardown's
read and its cancel, and that decision is correct to leave standing — forcing it
through would silently undo something that actually happened, which is exactly
the failure mode the whole design refuses elsewhere. `cancel_one`'s match arm
grew `StaleRevision` next to `NotFound`/`Terminal`/`AlreadyDecided` — a benign
race, not a bug to warn about.

**One clippy fight worth a sentence.** The natural shape for `check_revision`'s
return was `Result<(), (LedgerError, Vec<LedgerRecord>)>` — the entries it
committed riding home with the error. `clippy::result_large_err` didn't like it:
144 bytes on every `Result<_, LedgerError>` in the module, paid even on the `Ok`
path, for one method's payload. The fix ended up cleaner than the thing it
replaced — `check_revision` takes `&mut Vec<LedgerRecord>` and pushes into the
caller's own `all_committed` directly, the same contract `materialize_expiry`
already uses a few lines above it. `too_many_arguments` fired next (eight, one
over the limit) and asked for the same medicine every method with a git.push
transition needed for `Grant::from_terms` — a small `RevisionQuote<'a>` struct
bundling the four values the check actually needs, worth having on its own
merits since `id`/`quoted`/`by`/`attempted` read better named at a call site than
positional.

**What surprised me in the test file, not the source.** `ledger_core_tests.rs`
had four tests exercising exactly the shape revision-checking changes: post,
decide, decide again, assert the second decision's specific error. Every one of
them needed a second look, not a mechanical argument insertion — the *first*
decision in each pair correctly quotes the revision at post time (0), but the
*second* decision, now checked for staleness before its own legality, needed the
revision the first decision left behind, fetched fresh
(`approvals.get(&req.id).unwrap().request.revision`) rather than reused from the
stale local `req`. Get that wrong and the test still fails, but for the wrong
reason — `StaleRevision` instead of the `AlreadyDecided`/`Terminal` the test was
actually written to prove. The four sites were `granting_an_already_decided_
request_is_rejected`, both closed-state cases inside `terminal_states_reject_
any_further_transition`, `cancelling_a_cancelled_request_is_terminal`, and
`cancelling_a_granted_request_is_already_decided` — every test in the file whose
premise is "decide it twice."

`all_entries()` and its round-trip test in `kaish-types` — the fixture every
`LedgerEntry` variant round-trips through — turned out to be missing `Cancelled`
already, from R2, never caught because nothing in that file enumerates variants
against the enum itself. Added it alongside `RevisionRejected` while the tag list
was already getting a second entry; free coverage sitting one line away from
where I needed to be anyway.

---

## What deleting a clock cost, and what it bought (2026-08-09)

The design half of this is two entries down — "The kernel stops waiting", written
when the rule grew its third clause. This is what happened when the code caught up
with it, because two things surprised me and one ruling changed a contract kaish
has had since before the ledger existed.

**The first surprise: nothing was holding the orphans up.** §A.10 deletes the
request TTL, and the spec's own §H had already flagged the consequence — expiry was
covering for teardown. I read that as a housekeeping item. It is not. Before this
lane, `abandon_request` had exactly one production caller: the cancelled-grant undo
inside the decision chain. Discard a gated job with `kill --discard %1` and its
request stayed `Requested` forever. Kill every job, shut the kernel down — same.
Under a 60-second TTL none of that was visible: the orphan expired on its own and
returned its slot, so a missing teardown path cost a minute of capacity and nobody
noticed. Delete the TTL and the same missing path costs a live slot for the life of
the process — and *forever*, in an embedder where several kernels share one ledger
through `with_approver_handle`, which is exactly the shape kaijutsu is heading for.

So the lane grew a `ledger::teardown` module and four tests, one per row of §B.5's
obligations table, each asserting the live count returns to zero. Writing them
turned two rows into one call: a kaish session *is* a kernel, so "a session shuts
down" and "a kernel shuts down" are both `Kernel::shutdown`, and what separates
them is `ApprovalScope`. The test for that builds two kernels over one ledger with
different session ids, shuts one down, and asserts the other's request is
untouched — which is the only way to see the difference at all.

**The second surprise, and the ruling that followed it.** The ledger kept two
`Instant` mirrors — `request_deadline` and `grant_deadline` — computed at post and
grant time so a clock step could not move an expiry decision. There was a test
pinning it. §A.5 said something else in present tense: both surviving deadlines are
compared when observed, and a laptop suspend correctly makes a grant look expired.
Reading those side by side, I deleted the mirrors, wrote the wall-clock reading into
the spec as the settled one, and rewrote the test to assert it.

Amy rejected the framing. Not the deletion — the mirrors were genuinely protecting a
property nobody asked for — but the idea that kaish gets to say which clock is true.
"Wall clock" is a claim about what the reading *means*, and meaning is the embedder's
department, exactly like policy and deadlines and redaction. The kernel should care
that there is *some* clock, that there is exactly one of it per ledger, and that its
own view of it never goes backwards. Nothing more.

So the seam inverted. `WallClock` was `pub(crate)` with one production impl; it is now
`ledger::Clock`, public, installed through `KernelConfig::with_approval_clock`, with
`SystemClock` as the default — and saying the *default* reads system time is a fact
about the default, not a claim in the design. `Ledger::build` takes the clock as a
required argument rather than defaulting it, which is the cheapest way to make "one
clock per ledger" something you cannot get wrong.

The part I would not have thought of is the latch. The ledger now keeps the largest
reading it has taken, under the same mutex everything else commits under, and clamps a
smaller reading up to it. That is what lets the kernel hold no opinion at all: an
expired grant stays expired and entry stamps never regress *whatever* the installed
clock does, so the design never has to say what a well-behaved clock looks like. It is
the same shape as `sequence` — a property the record has by construction instead of one
a reader has to verify. The cross-family review pushed back that refusing to
acknowledge a clock step is itself a time decision, which is a fair reading; Amy
overruled it, and I think correctly, because the alternative is a ledger whose
invariants are conditional on the embedder's clock being sane.

The vocabulary sweep that followed was larger than the code change. "Wall-clock post
time" appeared on every entry field in `kaish-types`; `at` is now "the clock reading
this entry was committed at". The test clock stopped being `FakeWallClock` and became
`TestClock` — no longer a test double for a `pub(crate)` trait, but an ordinary
implementation of a public seam, which is a nicer thing for a test to be. I kept
`SystemTime` as the representation, and that is the one place a reader might still
hear "wall": it is `std`'s name, it is what RFC 3339 round-trips, and a neutral newtype
over it would ripple through three crates and both embedders to buy nothing the prose
does not already buy. The spec says so out loud rather than leaving the type name to
imply something.

**The ruling that changes a contract.** §I.5 asked whether a tool-level deferral
should halt the top-level loop the way a statement-level one already does. Amy
ruled halt. The case that settled it is not consistency, it is that the old
behavior let a *denied* operation's side effects run: `rm x; touch y` with `rm`
gated created `y` whether or not `rm x` was ever approved, and nothing un-creates
it. Exit 2 does not mean *failed*, it means *this has not happened yet*, and the
statements after it were written expecting it had.

The implementation is one branch. The interesting part is what it made unnecessary.
§C.1 had a whole subsection about `accumulate_result` overwriting the pending view
with `None`, and a proposed carry rule — keep the first pending request, let later
results add nothing. With the halt there is no "later result": nothing runs after a
gate, so the unconditional assignment is correct and the carry rule would be a
second mechanism for a case that cannot occur. Shipping both would have been two
answers to one question. The subsection is now four sentences saying why there is
no rule.

**And a rename I would not have bothered with alone.** §I.6: `Approver` → `Policy`,
`policy` → `evaluate`. Cosmetic in isolation — but after `decide` is gone the trait
has one synchronous method and approves nothing, while `ApproverHandle`, a
different object with a confusingly similar name, is what actually approves. It
costs a rename across the §F.2 table and it was free to take in the same lane. The
`Policy` snapshot test picked up an extra assertion on the way through: the trait
must contain no `async fn`. That is "the kernel never awaits an embedder" reduced
to something a compiler can check.

**One naming collision worth recording.** The spec puts `cancel` on `Kernel`.
`Kernel::cancel` already exists and means "interrupt the running execution". Two
unrelated cancellations under one name is exactly what the style guide's
one-term-one-meaning rule is for, so the kernel method is `cancel_approval` and
the spec now says why.

---

## Three things an embedder needed that `&` could not do (2026-08-07)

kaijutsu embeds kaish and, for background work, did not use it. Its
`background_exec.rs` spawns `/bin/sh -c` itself and says why in module docs:
a job started with `cmd &` "would be invisible to the *next* `shell` call —
its `JobManager` is gone", and "the job layer discards liveness". Both were
true. This is the work that makes them false, so that path can be deleted.

The first gap was the smallest and the most disabling. `Kernel::new` and
`Kernel::with_backend` each wrote `Arc::new(JobManager::new())` inline. The
private `assemble` already *took* a manager and there was already a `jobs()`
getter — every part of the seam existed except a way in. kaijutsu builds a
kernel per tool call, so every job it started died with the kernel that
started it. `KernelConfig::with_job_manager` is the whole fix. What it can
not fix, and what the field doc now says out loud: `kill_grace` and
`persist_output_files` are stamped onto the manager at construction, so on a
shared manager the last kernel built wins for both. Per-kernel views of
manager state would be a much larger change than the problem justifies.

The second reopened a decision from two days earlier. GH #240 found
`/v/jobs/{id}/stdout` promising a live stream in four places and delivering
one write at completion, offered two ways out — wire the tee kaish never had,
or remove the nodes — and Amy chose removal, because "the node bought a false
sense of watch-it-build for an MCP caller that would poll it and see nothing
until the build was already done." That was right while nothing needed it.
kaijutsu needs it: an agent starts `cargo build &`, gets control back, and
polls while it runs. So this is the other branch of the same decision, taken
because the requirement arrived — not because the first one was wrong. The
removal note had already sized the work correctly. `try_execute_external`
drains each pipe per 8 KiB chunk into a `BoundedStream`; the bytes only ever
lacked a second destination. `drain_to_stream_teed` gives them one, and
`self.bg_job_id` — already the seam that records a job's process group — says
which job to tee into.

Three limits came out of building it, and all three are documented rather
than smoothed over. Only a pipeline's `Only`/`Last` stage tees stdout, since
an upstream stage's output is the next stage's stdin. The completion write
fires only for a stream that received nothing live, because unconditional
would duplicate every byte and never would leave `echo hi &` empty forever.
And the one worth saying loudest: **a builtin is not a live producer.** It
returns a value when it finishes, not a byte stream — so
`cargo build 2>&1 | tee build.log &`, the exact command an embedder reaches
for, is not live, because kaish's `tee` is a builtin. The guidance that falls
out is better than the command it replaces: drop the `| tee`, the job's own
stream *is* the log. That only became visible because a test asserted
liveness on a pipeline and failed.

Which is the shape of every test in `job_live_output_tests.rs`. A real `&`
job emits, sleeps, emits; the assertion is that the first token is readable
while `status` still reads `running` **and** the second has not arrived,
sampling status before the stream so a completed job dumping its whole buffer
cannot satisfy it. #240's own tell was that the old `job_stream_tests.rs`
hand-wrote into a `BoundedStream` it built itself and read it straight back —
never once drove a real job. Reverting the tee fails five of eight here.

The third gap was not a kaijutsu observation but a grep: `PDEATHSIG` appeared
nowhere in the tree. kaish's three orphan guards — `setpgid` plus a pidfd
`killpg`, the cancellation cascade, and `kill_on_drop` — all need this process
to still be running code, so none of them survive `kill -9`, a segfault, or an
OOM kill. kaijutsu's own docs call `PR_SET_PDEATHSIG` "the deciding factor".

It is opt-in, and the split follows `vfs_budget_bytes` exactly: on for the
agent presets, off for `default`/`repl`/`transient`/`isolated`. An armed child
cannot outlive its shell and cannot opt out from inside — unlike SIGHUP, which
`nohup` and `disown` exist to escape. A human who backgrounds a long download
and exits the REPL expects it to survive; an agent embedder expects the
opposite. Neither is wrong, so the presets differ instead of one being
imposed. The flag rides on `ExecContext` rather than `Kernel` because
`dispatch.rs`'s spawn site reaches only the former, and one home is what keeps
the two `pre_exec` blocks from drifting.

`arm_parent_death_signal` also compares `getppid()` against the pid captured
before the fork, closing PDEATHSIG's documented race: a parent dying between
fork and `prctl` arms the signal against a parent already gone — the exact
orphan the flag exists to prevent, in the window hardest to notice. macOS is
documented as a gap and not faked; `kqueue`'s `NOTE_EXIT` needs a live watcher
process, which is the same dependency that makes the existing guards
insufficient.

Testing it meant a parent that really dies. The test re-executes its own test
binary as a helper, reads back the pid of an external the helper started,
`SIGKILL`s the helper, and asks whether the pid is alive. The negative case is
what makes the positive one mean anything: with the flag off the child must
*survive*, or something other than PDEATHSIG is doing the killing.

---

## The kernel stops waiting (2026-08-07)

This started as a comment in a parked test. The lease case on
`fix/approval-lease-expiry` had a long doc-comment explaining that
`LedgerConfig::request_ttl` was 60s while `Approver::decide_budget` was 300s, so
the default kernel handed an approver a five-minute budget to spend against a
one-minute lease, and a human who thought for ninety seconds lost their grant.
R2 already had the fix queued: delete the TTL, leaving one clock, and the test
goes green.

Amy read the comment and asked a different question. What if kaish returned data
to the embedder when the ledger blocks — what if the ledger were inverted so the
embedder controls the state?

The first thing to say is that the inverted path was already there.
`ApprovalOutcome::Pending`, `ResumeAction`, exit 2, `--confirm=<token>`, sixty-odd
tests. Stage 4 of the chain *was* the return path. So the question wasn't whether
to build one, it was whether to keep the other one — and once it was framed that
way, `Approver::decide` stopped looking like a seam and started looking like the
one place the spec violated its own spine.

§0.1 says the kernel owns mechanism and the embedder owns policy. Look at the two
trait methods side by side under that rule. `policy` is synchronous and pure: the
kernel *asks* a question and gets an answer back, and control never leaves. `decide`
is async and may take minutes: the kernel *runs* the embedder's policy, in the
kernel's task, on the kernel's clock, under the kernel's cancellation. Three
ownership confusions in one method, and §A.10 had already noticed the third one
without naming it — the patient hold got an explicit exemption as "watchdog
machinery," which is the sound a rule makes when it doesn't want to apply to
something.

The exemption doesn't hold. `decide_budget` decides. It decides that a decision
didn't happen. Which means R2-as-written would have fixed the instance and kept the
shape: one clock instead of two, still in the kernel, still bounding somebody else's
work.

So the rule grew a third clause: **the kernel never waits on the embedder.** Both
ways of waiting are wrong, and that symmetry is what makes it a rule rather than a
preference — a bounded wait is a clock-driven decision, which the previous clause
already forbids, and an unbounded wait is a liveness hazard the kernel can't cancel
on anyone's behalf correctly. There is no third option, so there is no hook.

**What corroborated it.** Two things, from opposite directions. Sol's deliberation
back on 08-05 had specced the kaibo-helper ACP bridge and found, independently, that
the ACP RPC must not be owned by the `decide` future because the kernel cancels it at
the budget — the bridge needed a correlation registry that survives cancellation.
That's a workaround for a hazard that only exists because the kernel owns the wait.
And §C.6 had already made this exact call once, for the classifier: *"The classifier
stays synchronous, and that is a decision rather than an oversight."* Don't put
embedder latency on the kernel's path; return instead. Applying the same reasoning
one section over wasn't a new idea, it was finishing an old one.

**What I expected to cost more than it did.** Resumption. Deleting an inline hook
sounds like it should cost mid-statement resumption, and it doesn't, because §B.4 and
§C.6 had already solved it for their own reasons. The statement tap fires
*pre-dispatch*, so a held statement never started and replaying it is free. A tool
gate captures `Capture::Exact`, so `rm a && dangerous b` replays only `dangerous b`.
Earlier statements' variables and cwd are session state and still hold. The pieces
were all there; nothing had needed to notice that they added up to a complete
resumption story without a callback.

**What it did cost.** The program remainder. A gated statement halts the top-level
loop (`kernel.rs:2947`), `confirm` replays that one statement and drops the rest of
the parse, and nothing anywhere retains a continuation — `JobStatus::Gated` turns out
to be a *finished* job kept alive so it isn't reaped, not a suspension. That was
already true, but `decide` hid it for the blocking case, and deleting `decide` sends
every case down the halting path. The answer is the same answer as everything else
here: the embedder submitted the program, so the embedder owns the remainder, and
`ResumeAction::ConfirmStatement` grew an `index` so it knows where to pick up. What
we didn't do is grow a continuation. That would be the kernel holding suspended
program state across an unbounded wait — the thing we just deleted, wearing a hat.

**And a real bug fell out of asking.** Tracing the halt turned up
`accumulate_result`: it assigns `accumulated.approval = new.approval` unconditionally,
under a comment asserting the pending view *is* the last statement's result. That
holds only when the gated statement happens to be last. `kaish -c 'rm x; echo ok'`
with `rm` gated returns exit 0 and no approval view, while the request sits live in
the ledger. Today expiry collects it after sixty seconds. After R2 deletes expiry,
nothing ever does — so removing a clock quietly converted a self-healing wart into a
permanent leak, in a completely different file. That is the kind of interaction I
would not have found by reading R2's own diff, and it is now §C.1's carry rule with a
red test named in R2.

There's a related question I deliberately did not settle: whether a *tool-level*
deferral should halt the loop the way a statement-level one does. It probably should
— exit 2 means "this hasn't happened yet", and the statements after it were written
assuming it had — but that changes behavior an existing script can see, on the widest
contract kaish has. It's §I.5 with a recommendation and Amy's name on the decision.

**The shape of the answer, stated once.** The thing to invert was the *wait*, not the
ledger. Amy's phrasing was "invert the ledger so the embedder controls state", and the
distinction that matters is that two different states were wearing one word. The
suspended decision — who's waiting, on what UI, for how long — inverts, completely.
The record and the balance rule do not: one grant, one successful settlement, revision
counters, replay binding. Those are the parts that must be correct under concurrency
and identical for everyone, and inverting them would make every embedder re-implement
the hard part, which is the opposite of what an embedder-controlled design is for.
The kernel keeps the ledger. The embedder gets the clock, the task, and the decision.

Written up as CLAUDE.md's "The embedder is in control", because this stopped being an
approvals question about a third of the way in.

---

## The statement gate: recording what was asked, before anything runs (2026-08-05)

Ledger PR 10, and the first layer that watches *statements* rather than paths.
The `fs.*` layer records what a command touched. This one records what was
asked to run, before any of it runs — which means the record has to be built
out of the parse, not out of execution, and that constraint decided almost
every question that followed.

**The plan is what a classifier reads, so it renders unexpanded.** `${HOME}`
stays `${HOME}`; `$(cat list)` stays `$(cat list)`. There was an existing
renderer — `Kernel::format_expr` — and the temptation was to extend it. It
turned out to be the wrong shape: `format_expr` renders a job listing, so it
drops what it cannot show ("...", `"..."`, `<<heredoc`). A plan is an audit
record and a policy input; a `_ => "..."` arm there is a silent hole in both.
So the plan renderer is a parallel AST walk with no fallback arm, mirroring
the sexpr formatter's coverage, and the 8 KiB truncation the spec asks for
carries a marker that names the number. The structure survives the cut — a
classifier that needs more than 8 KiB of *text* is reading the wrong field.

**The tap fires at exactly two sites, and the tests are what hold it there.**
The top-level statement loop and `execute_argv`. The design review settled
this before implementation, and after building it I understand why it needed
settling: `execute_stmt_flow` is the obvious place, it is one function, and it
would post a thousand entries for a thousand-iteration loop. The rule is easy
to state and easy to violate later, so the matrix has a test for the loop (one
entry), for a user tool's body, for a `$(…)`, and for a sourced script — each
of which would light up if someone "simplified" the tap into the recursion.

**Two bugs the tree found that the spec had not.** The first was ours the
moment we wrote it: the plan rendered `rm --confirm=<the real token>` straight
into the ledger, and §A.2 says no entry carries a credential. The existing
`no_projection_contains_the_issued_credential` test caught it on the first
full run — the value of a test that scans serialized bytes for the issued
token rather than asserting on a field list. The renderer now redacts the
`confirm` argument's value in both spellings (`--confirm=` and `dd`'s bare
`confirm=`), keeping the fact that a key was presented and none of the key.

The second was structural. `ObservedResource.subscription` was a required
`SubscriptionId`, and the statement tap has no subscription — `cmd.*` never
enters the registry, by design, so the classifier stays the sole posture
decider. A sentinel id would have been a lie in an audit record, so the field
became `Option`, `new` kept its meaning, and the tap got its own `planned`
constructor. That is the second time in this ledger series that "what do I put
here when there is nothing" was the question that found the real design.

**Always-on had to be paid for honestly.** PR 8 shipped a test asserting an
unsubscribed 10,000-path delete posts *zero* entries. That assertion is now
false — it posts one, for the statement. Rather than filter the tap out and
leave the claim looking untouched, the test states the new number: 1 entry and
0 approval requests for 10,000 paths. The load-bearing property was never "the
log is empty", it was "the cost is not per path", and that still holds. The
`fs.*` matrices do filter the tap out, because those tables are about the fs
chain and always were.

**Replay by index.** A held statement has no source span to slice, so the
capture is the program source plus the statement's index — `confirm` re-parses
outside the execute lock (pure computation), then runs exactly that statement
under the lock. Earlier statements already ran in that session and their
variables and cwd still hold, which is what makes `target=chosen.txt` on line 1
visible to the replayed `rm ${target}` on line 2. The sharp edge was the
redemption context: a `confirm` of an *fs.remove* also passes through the
`execute_argv` tap site, and if the statement gate took that correlation the
inner `rm` gate would find nothing waiting for it. The site peeks at the
granted operation instead of taking the correlation, and leaves anything that
is not `cmd.execute` alone.

**The measurement, not the assertion.** §C.6 says the reference classifier
ships with a measurement of whether the plan discriminates better than the raw
line. It does: 9/9 against 6/9 over a small labeled corpus, disagreeing on
`echo 'rm target.txt'`, `grep rm changelog.txt`, and `cat rm`. The test asserts
plan ≥ raw and requires at least one disagreement rather than pinning the
percentage — the number belongs to the corpus, and a hard-coded one would rot
the first time someone adds a case.

**The review found the thing I never thought about: `--confirm`.** The
pre-merge review came back NOT READY with two blockers and one root cause — the
statement gate did not interact with the redemption key at all. Two symptoms,
one bug wearing two coats. Functionally, a user handed a key for a held
statement re-runs the line with it and *nothing happens*: the gate hardcoded
`presented: None`, so it saw no key, minted a second request, and deferred
again with the first still pending. There was no other statement-level way to
redeem. And because the re-run minted a request, its
`Capture::Statement.source` — the line as typed — carried the live key into
`ApprovalRequest.capture`, which reaches the view, `/v/approvals`, and
`LedgerEntry::Requested`. I had redacted `Plan.rendered` and stopped one field
short, which is the tidiest illustration I have of why "I redacted it" is not
the same claim as "the record carries no credential".

The fix is one walk because it is one rule: lift the literal key out of the
argv, hand it to the gate, redact it from the rendering, remove the token from
the captured source. Making one predicate decide all three fixed a wrinkle the
first cut had — `--confirm=${key}` was rendering as `<redacted>` even though an
unexpanded plan never held the value. Now a literal key is redacted and a
variable renders as written, which is the honest rule: **what the record cannot
see, it cannot leak.**

Then reviewing my own fix found the case *it* missed. The capture records the
whole program source, so redacting the held statement's key leaves an earlier
line's key sitting in it — `rm --confirm=<key> a` on line 1, `rm b` held on
line 2, and the credential rides along one statement over. The scan is
program-wide now. Two rounds on one seam, each finding the same class of bug
one scope wider; the lesson worth keeping is that "where does this string end
up" deserves the same walk as "where does this string come from".

The review's medium was a real hole too: `confirm`'s `result?` could return
before the settlement below it, so a replay that *errored* left its attempt
`Reserved` until the sweep abandoned it — every later redemption of that grant
failing `AttemptInFlight` in between. Finding a statement that errors rather
than exiting non-zero took three tries (arithmetic in argv is an exit code; a
glob with no matches is an exit code; an assignment whose right-hand side will
not evaluate is an error), and that hunt was worth more than the fix: kaish
converts most failures into exit codes on purpose, and the few that stay errors
are exactly where an invariant like this one hides.

On the low finding — a panicking classifier — I chose documentation over a
guard, and documented it on `Approver` too. kaish does not `catch_unwind` an
embedder hook anywhere; the tree's answer to "a hook died" is drop-safety, not
a swallow. Guarding one of the two hooks would have been the worst of both.

## Shutdown that doesn't, and a job list that doesn't sort itself (2026-08-05)

Same job-system design audit as GH #240, two more findings from the same
2026-07-30 sweep: `Kernel::shutdown()` and the sharp edges around it.

`shutdown()` called `JobManager::wait_all()` and nothing else — no timeout,
no cancellation. `sleep 3600 &` then `shutdown()` blocked for an hour. Worse,
the trait method an embedder would actually reach for,
`EmbeddedClient::shutdown`, was a no-op with a comment claiming the kernel's
`Drop` would clean up background jobs. It wouldn't have, even if `Kernel`
had a `Drop` impl (it doesn't): a background job's task holds its own
`Arc<Kernel>` fork, freshly minted by `fork_for_background`, not a reference
back to the kernel the embedder holds. Dropping the parent's `Arc` doesn't
touch it. So the embedder's actual choices were hang or leak — the audit's
phrase for it stuck.

The fix mirrors the kill-lifecycle precedent from PR #284: cancel every
job's token first (`cancel_all_jobs`, new), then wait `kill_grace + 3s` **per
job**, same bound `kill %N` gives one target, logging and abandoning
whatever doesn't unwind in time. Wiring `EmbeddedClient::shutdown` to it
needed one signature change first — `shutdown` took owned `self`, but
`EmbeddedClient` only ever holds `Arc<Kernel>`, and `Arc::try_unwrap` had no
reason to succeed with background-job forks in the picture. Nothing in
`shutdown`'s body actually needed exclusive ownership — it was always
working through the shared `Arc<JobManager>` — so `&self` was the honest
signature, not a workaround.

Chose not to add `impl Drop for Kernel`. It was tempting: cancellation is
synchronous (`CancellationToken::cancel()`), so a Drop impl could fire it
without needing `.await`. But iterating jobs to find their tokens needs the
`Arc<JobManager>`'s async mutex, and the only sync-safe way to touch it from
Drop is `try_lock()` — best-effort, silently skips cancellation if anything
else holds the lock at that instant. The issue's own shape-of-fix never
asked for Drop, and a Drop impl whose behavior depends on lock contention
timing is exactly the kind of silent inconsistency this project doesn't
want; documented the gap in EMBEDDING.md instead (call `shutdown()` before
dropping a kernel that might have background work — there is no automatic
safety net).

Two adjacent items rode along from the sharp-edges issue (#247) rather than
stretching this PR to cover all five: a panicked background job's task
drops its oneshot sender without a result, and that arm of `try_poll` read
`failed:1` with the text `"job channel closed"` — indistinguishable from an
ordinary failing command, and never logged. Now it's `tracing::error!`'d and
says what actually happened. And `JobManager::list`/`list_ids` iterated a
`HashMap` directly, so two jobs could list as `[2, 1]` — cosmetic in a REPL,
a real flake source for an MCP caller or a snapshot test. Sorted by `JobId`
now, which `kaish_types` teaches to order correctly (`Ord`/`PartialOrd`,
ids are minted strictly increasing, so ascending is spawn order).

One item from #247 turned out to already be fixed and not by this PR: the
issue described `kill %N` on an embedder-registered job (no cancel token
attached) as reporting `"not found"` — actively misleading, since the job is
right there in `jobs`. A throwaway probe test against current `main`
(register a job via the public `JobManager::register`, then `kernel.execute
("kill %1")`) showed the real message: `"kill: job 1 has no cancellation
token and no live process group — nothing to deliver termination to"` —
exactly right, not "not found". GH #244's kill-lifecycle work must have
closed this between the audit and now. Worth the reminder: verify a claim
against current code before building on it, even one from the same issue
that's otherwise still live.

Left out, deliberately: #247's items 4 (document the tokio-runtime-pinning
trap — done, folded into this PR's EMBEDDING.md pass since it was one
paragraph) and 5 (`StreamStats` isn't serializable — the issue itself says
fold it into whichever PR adds serde to the job types; this wasn't that
PR). Neither needed code changes here.

## The watcher loses its paperwork (2026-08-04)

Four days after PR 8 merged, its post-merge review took the observe design
apart — and Amy's question mid-review took it further than the reviewers did.

The review first. deepseek and gpt read the merged tree independently, and
every correctness finding pointed at the same seam: the *second* glob matcher.
The gate site's filter classified each path against the resolved form; then
`record_observed` posted a request carrying display paths, and the decision
chain's stage 1b re-matched the subscriptions against *those*. Two matchers,
one question, two different answers. A relative path under observe exited 1
claiming the subscription was gone (it wasn't — the path spelling had
diverged). A batch spanning two disjoint observe subscriptions exited 1
because the auto-grant was all-or-nothing per subscription. And — gpt's
find, the worst of the three — stage 1b considered only observe rules, so an
enforce-classified request could be auto-granted by an overlapping observe
subscription. The gate downgraded to a note, silently, which is the exact
sentence the precedence rule in §C.5 was written to forbid.

We were mid-conversation about ledger use cases when the findings landed, and
Amy asked the question that reframed them: *"can filesystem be hooked without
the ledger for observability only? that would probably be a big bulk."* She
was right on both counts. Record-only observation was paying for authorization
bookkeeping it never used — four entries per covered batch (`Requested`,
`Granted{Observe}`, `Redeemed`, `Settled`), a credential minted for a grant
nobody could redeem by hand, live-index traffic, and that second matcher — all
to write down a fact nobody decided. fanotify has this distinction exactly:
notification marks stream events; only permission marks block for a verdict.
PR 8 quoted that analogy and then built a permission mark that always says
yes.

So the fix deleted the machinery instead of repairing it. An observed
mutation now posts one chainless `LedgerEntry::Observed` — per resource: the
display path the command named, the resolved path the glob matched, and the
covering subscription's id. The gate site's classification *is* the decision;
there is no second matcher left to disagree with the first. Bugs one and two
didn't get fixes, they got deleted, and with stage 1b gone from the chain,
bug three's code path does not exist — the only things that can grant a
request are standing grants, policy, decide, and out-of-band authority, which
is the sentence §C.2 wanted to be true all along.

Two things worth keeping from the exercise. First: the review pattern held
for the sixth PR running — a clean-looking merge, then real bugs behind it,
found by models reading the whole tree rather than the diff. PR 8's own
mutation table proved the filter; nothing proved the handoff, and all three
bugs lived in the handoff. The new regression tests pin exactly that seam
(relative path, disjoint subscriptions, enforce-observe overlap, end to end
through `kernel.execute`). Second: when a review finds three bugs in one
structure, the structure is the finding. The instinct to patch all three in
place would have left the two-matcher design intact and the next drift
between them unfound.

This entry rides the refinements PR; the chain-backed design it replaces
lived on main for four days and never shipped in a release.

## The job stream that was never live (2026-08-05)

GH #240 named a gap a job-system design audit found and a second model
family confirmed: `/v/jobs/{id}/stdout` and `/stderr` promised a live
stream in four places (`jobfs.rs`'s own module doc twice, `job.rs`'s
`persist_output` rationale, `docs/LANGUAGE.md`) and delivered something
else entirely — the whole buffer arrived in one write, at completion,
because `execute_background` ran the pipeline to the end before writing
`result.text_out()`/`result.err` into the job's `BoundedStream`s. Nothing
ever teed a running child's output into them mid-flight. The tell was in
the test suite: every `job_stream_tests.rs` case hand-wrote into a
`BoundedStream` it constructed itself and read it straight back — not one
drove a real `&` job through `kernel.execute()` and peeked the stream
while it was still running.

Two ways to close the gap: wire the tee kaish-kernel never had (thread the
job's stream down through `try_execute_external`'s drain tasks so bytes
land as they arrive), or admit the promise was never load-bearing and
remove it. Amy chose removal. The node bought a false sense of "watch it
build" for an MCP caller that would poll it and see nothing until the
build was already done — worse than not having the feature, since it
looks like it should work.

Once the VFS surface was gone, `Job::stdout_stream`/`stderr_stream`,
`with_streams`, `register_with_streams`, and `JobManager::read_stdout`/
`read_stderr` had no remaining reader in production — kept, they would
have been dead code serving a deleted node. Removed the whole path rather
than leave a write-only apparatus behind, which meant `execute_background`
stopped constructing per-job `BoundedStream`s at all.

The harder question was what a caller does instead. `job.result` already
holds the captured stdout/stderr text after completion — it always did,
that's what `wait` gates on — but nothing surfaces it as text through the
VFS or `JobInfo` once `/v/jobs/{id}/stdout` is gone. A hermetic kernel
(`persist_output_files(false)`, no host temp file either) genuinely loses
the ability to retrieve a background job's output through any kaish
surface unless the caller redirects explicitly: `cmd > /tmp/out &`, then
`cat /tmp/out` after the job's `status` reads `done:`. That works today
(background jobs inherit cwd/env and run real pipelines, redirects
included) and became the replacement pattern in every test and doc this
PR touched — `docs/LANGUAGE.md`, `docs/EMBEDDING.md`,
`kaish-help/content/en/vfs.md`, and the `background_execution_tests.rs`/
`concurrency_tests.rs` cases that used to `cat /v/jobs/N/stdout`. Worth
flagging for whoever picks up the "fake `/proc`" idea later: Unix's own
answer to "read another process's live output" is `/proc/<pid>/fd/1`, not
a shell-owned convenience node — a future live surface should probably
follow that shape rather than resurrect this one.

`job_stream_tests.rs` — the file the issue named as the reason this
survived — is gone; its status/command/list coverage moved to
`job_vfs_tests.rs` with the stdout/stderr fixtures deleted rather than
repaired, since there is nothing left to fix them toward.

## The ledger learns to watch without stopping you (2026-08-03)

PR 8 of the nine. The feature is small and the constraint is the whole job:
`fs.*` observability subscriptions — a glob that says "record every mutation
under `/workspace/**`" without turning a single write into a prompt.

Everything in §C.5 hangs off one sentence: *free when nothing is subscribed.* A
recursive delete over a large tree is a first-class kaish workload, and a ledger
that taxes it by default is a ledger operators turn off — at which point the
audit trail nobody paid for is also an audit trail nobody has. So the question I
kept asking was not "does the filter work" but "what does the filter cost when it
has nothing to say".

Three answers, in order of how much they mattered:

- **One relaxed atomic load.** `LedgerInner.any_subscriptions`, written under the
  lock and read without it. The alternative — asking the registry — takes the
  ledger's single mutex, which would serialize every filesystem operation in the
  process against every other one to learn that nothing is subscribed.
- **One snapshot per command, not per path.** `SubscriptionFilter` holds the
  registry for the length of one gate call. A per-path query would acquire that
  same lock ten thousand times on one delete.
- **A counter, because "free" is a number.** `ApprovalRequest::constructed_count()`
  increments in `ApprovalRequest::builder`. The test creates 10,000 files, deletes
  them with a glob that really does hand the gate site 10,000 positionals, and
  asserts the delta is 0. It lives alone in its own integration binary — the
  counter is process-wide, and a neighbour that gated anything would turn a real
  regression into a flake.

Then the tests found the thing I had not thought about. The gate site classifies a
path as observed from its snapshot; the decision chain re-derives the same answer
under the lock. Two reads of one question — and when they disagree, the observe
request *defers*, so the command exits 2 with a grantable-looking prompt attached
to an operation that has no permission semantics at all. The mutation that exposed
it: make `enforce` behave like `observe` in the filter. Every test still passed.
The enforce test's exit 2 was arriving from the deferral instead of from the gate —
the right answer for the wrong reason, which is precisely what a test suite exists
to notice and this one did not.

The fix is a refusal to paper over it. `record_observed` gates its draft directly
and treats a deferral as **exit 1**, naming the disagreement: either the
subscription was revoked while the command ran, or the filter and the chain's
stage 1b have drifted apart. Exit 2 would advertise a request an operator could
grant, for a decision nobody is making.

Four things the spec left to the implementation, decided and written into §C.5 so
PR 9 does not have to rediscover them:

1. **`enforce` beats `observe`.** The stronger posture wins; the other order could
   quietly downgrade a gate to a note.
2. **Subscriptions match per resource, not all-or-nothing.** A standing grant is
   all-or-nothing because it authorizes; a subscription only scopes. §C.5's own
   worked example — record `/workspace/**`, stay silent about `/tmp/**` — is
   unreachable any other way. So the gate site partitions its paths by posture and
   posts the observed ones as their own request; the auto-grant that closes that
   request is still all-or-nothing, because it *is* a grant.
3. **Match the resolved path, record the display path.** A scope a relative path
   could step outside of is not a scope. The record still names the string the
   command wrote, because that is what an auditor is trying to recognize.
4. **Observe fires for every mutation the glob covers**, including the ones a gate
   would never have held — a new file, an append, a delete the trash caught. §C.5
   asks for a record of every filesystem mutation, and one that skipped the
   survivable ones would be answering a different question.

The one thing I did *not* do: give `subscribe` a shell surface. A session that
could subscribe itself could also unsubscribe itself, and an audit scope an agent
can turn off is decoration. It stays on `ApproverHandle`, where the embedder holds
it.

## The cutover: the latch becomes the ledger, and the word goes with it (2026-08-02)

PR 5 of the nine — the one the plan called "aggressive-clean: no compatibility
step, one cutover". Delete `NonceStore`, rewrite ten gate sites on
`request_approval`, apply the §F.2 rename table, land `Kernel::confirm` on the
ledger, and carry the two pieces of §F.3 hardening that belong with a cutover
rather than with the thing being deleted.

**The census earned its keep before a line was written.** A fresh map of every
site — ten gate sites, thirty-nine `.latch` control-plane hops, the eleven-hop
confirm path, both `set` parse paths, ~110 tests — turned up the single biggest
under-scoped item in the plan: `ExecResult.approval`, added by PR 3, had **one
writer and one reader**. `.latch` had thirty-nine hops. So this was never a
rename over an existing mesh; the `.approval` control plane had to be built from
nothing, and every one of §F.2's "stable — does not move" guarantees (survives
`clear_stdout`, survives the `ToolResult` roundtrip, overrides a later pipeline
stage, rides scatter rows, survives `--json`) was *false* for `.approval` on the
morning of the cutover. Budgeting for that up front is the difference between a
day and a week.

The census also caught three stale line citations in §F.3 and an "8 hex" claim
about a nonce that has been 32 hex since #259. Small things, fixed in the PR
that made them visible — but the kind of small thing a changelog quotes verbatim
if nobody looks.

**Where I deviated from the spec, and why.** §F.1 said `gate_overwrites` keeps
its signature. It gained a `KernelOperation` parameter instead, because §A.6
wants adding a gate site without registering its operation to be a *compile*
error, and the alternative — sniffing the command name inside the helper —
picks a plausible wrong default in silence. Those two sections wanted opposite
things; the one that fails loud won.

**The draft matcher is what makes a bearer key safe to be wrong about.** A
presented `--confirm=<token>` does not name its request. Under the latch that
was fine, because the nonce *was* the identity. Under the ledger a wrong key has
to count against *some* request or the rejected-attempt limit has nowhere to
attach — that is exactly what #259 deferred for want of an attempt-identity
model. So the draft names the request: same operation, same resource set,
newest match wins. It deliberately matches *closed* chains too, which is what
lets a key presented after a successful run report what already happened
instead of quietly posting a fresh request and deleting the file a second time.

**Two tests had to invert, and that inversion is the whole point.** The latch's
nonce was reusable inside its TTL, so `rm.rs`'s `test_rm_latch_nonce_reuse_idempotent`
and its integration-test twin asserted that re-presenting a key was a harmless
no-op. It was not harmless: it ran the delete again. Both now assert the
opposite — the settled outcome is reported and the file is deleted exactly once
— and the test that proves it restores the file between the two presentations,
because an assertion that the file is gone would pass either way.

**One test I deleted rather than weakened.** `confirm_without_captured_invocation_errors`
hand-built a `LatchRequest` with an empty tool and argv. The type is gone, and
every request a kernel-routed command can raise goes through the dispatch seam
and captures `Capture::Exact` — there is no honest way to reach the non-`Exact`
refusal from that file. Faking one would have tested the fake.

**What implementation found that the plan could not have.** §F.3 worried that
`$(set +o latch)`, `set +o latch | cat`, and `set +o latch &` could route around
a policy pin through the parser's flags-versus-positional quirk. They cannot:
`set` is a *grammar keyword*, so all three are parse errors before any policy
check runs. That is a stronger guarantee than the refusal — and a fragile one,
because it belongs to the grammar rather than to the pin. The cases stay in the
matrix accepting either outcome, so the day `set` becomes an ordinary command,
the pin is what catches them and the test says so.

**The word went too.** Mid-flight, Amy resolved §I.4: the latch is retired
completely rather than leaving two spellings of a word the rest of the design
drops. `set -o latch` → `set -o approvals`, `JobStatus::Latched` → `Gated`
(wire `"gated"`), `KAISH_LATCH` → `KAISH_APPROVALS`. `Kernel::confirm` and
`--confirm=<token>` stay, because "confirm" was never latch vocabulary — it
names what the operator is doing, and that has not changed. Arriving mid-PR, it
cost a mechanical sweep and a full rewrite of the help fragments' Latch section
into an Approvals section; arriving after the PR it would have cost a second
breaking release.

**A small thing I got wrong twice in the same afternoon.** Two tests asserted
the tokenless boundary by grepping the serialized view for the substring
`"token"`. Both fired — on the hint's literal `<token>` placeholder, which is
display text. §A.2 promises that no *field* is a credential, not that no field
*mentions* one. Walking the object's keys is the assertion that matches the
promise; the substring check was testing a coincidence.

---

## The ledger lands in lanes, and the second family earns its seat (2026-08-02)

Four PRs of the approval ledger's nine-PR plan landed in one day — types (#274),
core (#275, hardened by #277), ToolCtx (#279), and the Approver chain (#278) —
built by four agent lanes across two overlapping sessions, and the day's real
story is what the review structure caught.

The spec work was Amy's: the redraft (#271) settled the decisions the lanes then
built against without re-litigating — a pure-bearer key with accountability in
the `KeyRetrieved` record rather than key-holder identity; one successful
settlement per grant, universally (`max_redemptions` deleted; re-presenting a
key after success reports the settled outcome and never re-executes);
`req_<8hex>_<seq>` ids with no short form; and authority in the signature —
`Kernel::build → (Kernel, ApproverHandle)`. Every lane's "decisions made where
the spec was ambiguous" section traces back to how much that pre-work
constrained the search space: the ambiguities left were real ones, worth
flagging, and none contradicted a settled call.

The core PR is why two-family review is now the standing bar for foundational
ledger PRs. The building lane's own pre-PR review (or-glm) found two
commit-or-nothing violations and the PR merged green — and then a second review
round from a different family (gpt) came back "do not merge as-is" with four
blockers or-glm had cleared: the clock sampled before the lock (a contended
caller decided against the instant it called in, not the instant it committed);
a sweep that could `mark_closed` twice and quietly widen the live-capacity
gate; terminal entries refusable by ring/sink capacity — an operation that
already *ran* could be told its own settlement didn't fit, leaving the attempt
unsettleable forever; and condition-widening never validated. The merge had
landed mid-round (two sessions, one branch — the coordinating session watched
the "merged" worktree's files change under it and correctly refused to clean it
up), so the fixes landed as #277 minutes later. No harm done, but the ordering
lesson stuck alongside the review lesson: verify a lane is actually finished
before treating its PR as finished.

The pattern repeated on the parallel lanes, gpt catching what deepseek cleared
both times: on #279, a dropped guard's undrained settlement could make the next
request see a falsely-full ledger, and a `#[doc(hidden)]` constructor whose doc
claimed forged handles "fail loud" when `settle` never checked ownership; on
#278, a cancellation landing between the post-`decide` check and the ledger
commit still posted a `Granted` entry. That last one got the day's best fix
shape: the window can't close without threading cancellation through the
ledger's boundary, so the guarantee was restated as an outcome — a cancelled
execution never leaves a live grant; `undo_if_cancelled` abandons the request
loudly and the record keeps both the decision and its undoing.

Two lanes also paid a full CI round trip each for the same one-line failure:
rustdoc's `-D warnings` leg catching a broken intra-doc link that all five
documented local gates miss. CLAUDE.md's Build Commands now carry the doc gate
with the reason attached.

The lanes converged cleanly: the PR3×PR4 collision came down to one conflict
worth thinking about — PR 4 refactored `grant` into a delegation to
`grant_with_grounds` while PR 3 pushed `drain_outbox()` into `grant` — and the
drain moved into the delegate, because the delegate is what appends. Left on
the books, flagged in the PR bodies rather than fixed: `commit_terminal`'s
unreachable fallback traces under the ledger guard (a deadlock path exactly
when diagnosing an invariant failure), and `renew` doesn't re-observe
transitions until PR 6's resolver exists. Next: the PR 5 cutover — the latch
becomes the ledger, `NonceStore` is deleted, and the census of everything that
touches is regenerated fresh rather than trusted from a dead session's
scratchpad.

## The review cascade: every layer of stale prose hid a bug (2026-08-01 → 2026-08-02)

It started as a writing exercise. Amy asked for a Simplified-Technical-English-style
prose reset — a small, predictable subset of English to match kaish's small,
predictable subset of `sh`. That landed as `docs/style.md` (#260), dialed in by eval
before it shipped: Sonnet and Opus each rewrote `help/limits.md` under the draft
guide and both got *longer* (Opus +17%), which killed the guide's 25-word sentence
budget; deepseek and or-glm critiqued it against the real corpus and caught six guide
bugs, including a spine sentence asserting the exact ShellCheck overclaim that #239
was retiring the same day.

Then the exercise kept returning correctness bugs. Every layer of stale or padded
prose we opened had a factual error inside it: three code comments that were false
rather than merely stale, a help file promising full output it no longer delivers,
two builtins publishing a PID path that does not exist, an unsupportable threat model
in a security comment, and a breaking change buried mid-bullet where nobody scanning
for `**BREAKING:**` would find it.

So we swept. kaibo over everything merged since v0.13.0 — deepseek as the workhorse,
or-gpt on the concurrency and security surfaces — and the sweeps produced #268: `kill
%1 %2` signalled job 1, dropped job 2 silently, and exited 0; a single Ctrl-Z hung
`Kernel::shutdown` forever (a stopped job can never become done, and `wait_all`
polled it at 10ms with no timeout); the bg reaper waited without `WUNTRACED`, so a
re-stopped job showed Running forever; and `apply_spill_contract` clobbered
`original_code` when applied twice.

The cascade is the part worth remembering. #268's own review confirmed all four
fixes and found the residual: the `wait_all` stopped-skip is a snapshot, so a job
stopping *after* the filter — the new WUNTRACED reaper observing a SIGSTOP is
exactly such a path — re-created the hang through a sub-200ms window. That became
#270, which moved the guard inside `JobManager::wait`'s poll loop where it also
fixed a day-one hang nobody had ever hit deliberately: `wait %N` on a Ctrl-Z'd job
now fails loud with the resume instruction instead of polling forever. Meanwhile
the gemini-pro batch from the same sweep produced #269: `grep -c` over multiple
files printed one aggregate total while the comment directly above the code claimed
GNU per-file behavior. The comment recorded the intent; the code was the bug — the
same comment-versus-code disagreement as everything else in the sweep.

Each review's findings became the next PR, and each PR got its own review before
merging; the cascade terminated when a review came back with only noted-for-later
items (a `kill` test against a stopped middle target, an all-stopped `wait_all`).
The one big deliberately-open item from the sweep: `fg` and the bg reaper can both
`waitpid` the same pid, and the loser's `ECHILD` maps to `Exited(0)` — success
reported for a process that stopped. That wants a single wait-owner per OS job, and
it is on the books, not in this batch.

Nineteen PRs landed across the two days (#239–#270 range), including the style
guide's adoption by reference in kaish-extras (extras#6) and kaibo (kaibo#117), and
the approval-ledger design doc's migration into kaish as the living canonical copy
(#266) — that story gets its own entry when the redraft lands.

## We optimized for a year against a profile we never ran (2026-07-30)

GH #48's own "Approach" section says: *profile a representative workload first,
"treat the bullets above as hypotheses to confirm, not a task list."* Then a
gemini-pro + fable batch review produced a ranked ten-item burndown from a static
read of async frame sizes, and we landed eight of them (items 0–5, 7, 8) without
ever running the profile. The two we didn't land — item 6 (`Value::Json(Box<…>)`)
and item 9 (split `ExecContext` into `Arc<SharedCtx>` + a mutable core) — were the
two the review itself flagged "measure first, then decide". Amy sent me to go
measure.

The harness is `crates/kaish-kernel/examples/alloc_profile.rs` behind a `dhat-heap`
feature. dhat over a hand-rolled counting allocator for two reasons: the workspace
denies `unsafe_code`, so a `unsafe impl GlobalAlloc` of our own would mean punching
a hole in that for a profiling toy, and a counter gives totals with no attribution
— attribution was the whole point. `required-features` keeps the example (and the
dependency) out of every normal build, test, and clippy run. It parses dhat's own
JSON back in-process and prints a ranked table, so a terminal-only run answers
"where did the allocations come from" without the external viewer. Six workloads,
all shaped like an embedder rather than a REPL: kernel construction, one
`execute()` per tool call, grep-over-a-tree, nested `$( )`, a 200-command loop,
and an 8-way scatter/gather.

**The profile contradicted the review on both parked items, and on where the cost
actually is.**

Item 9 is simply not there. `snapshot_exec_ctx` — precisely what the split
collapses — never exceeds 10.3% of allocations in any workload, is 7.5% in the
loop workload where per-command dispatch is most exposed, and 1.0% in the
per-`execute()` workload. Worse for the proposal: much of even that is the
`Scope::clone` *inside* the snapshot, and `scope` is the mutable half the split
leaves behind. The read-mostly fields it would collapse are already `Arc` bumps
costing zero allocations. Widest change on the list, single-digit payoff.
Declined on allocation grounds.

Item 6 is more interesting: `Value::Json` *is* the top term in scatter/gather
(53.6% of blocks, 39.3% of bytes) — and boxing it would not help at all. The cost
is a **deep clone**, not a size: `Scope::last_result` carries the previous
command's whole `ExecResult` including `.data`, and every `Scope::clone` (three
per command, plus one per fork) deep-copies that JSON through indexmap. A `Box`
clone is still a deep clone, one pointer further down. The review also predicted
`Value` at ~88 B (it's 72) and predicted the win would land in the recursion
frames — in the actual recursion workload, `serde_json::Value` cloning is 0.0%.
The change the evidence supports is `Arc<serde_json::Value>`, which is a different
change with different semantics, and Amy's call. (The narrower fix — don't carry
`.data` in `last_result` — is blocked: `kaish-last` reads exactly that.)

And the thing nobody predicted, which is now the single biggest number in the
report: **`parser::parse()` rebuilds the entire chumsky combinator graph on every
call.** For one `echo hello world` through `kernel.execute()` — the exact shape of
a kaibo or kaijutsu tool call — that is 840 allocations and ~163 KB, **62% of all
allocations and 69% of all bytes, spent before a single token is consumed.**
Nothing in the burndown touches it, and it can't be fixed cheaply: chumsky 0.13's
parser type is parameterized by the input lifetime, so the built graph can't be
stashed in a `OnceLock` without a transmute we've denied ourselves. Filed rather
than forced.

Three cheap wins did fall out of the profile and landed here. `grep` built a fresh
`grep_searcher::Searcher` per file, and a `Searcher` owns a 64 KiB zeroed line
buffer plus an 8 KiB decode buffer — 94 MB across 64 files, 68.5% of every byte
the grep workload allocated, for a buffer `search_slice` resets anyway; one
searcher per walk cut the workload's bytes 70%. `glob_match` ran brace expansion
unconditionally, five allocations per call for a pattern with no braces — and
every ignore rule against every walked path goes through it, which made it 49.7%
of the grep workload's allocation *count*; a brace-free fast path took 41% off it.
And command dispatch called `tool.schema()` per command, which for a clap-derived
builtin rebuilds the whole clap `Command` and reflects it — to produce exactly
what `ExecContext.tool_schemas` already held, name-sorted; a `binary_search_by`
into the catalog took that from 69,850 blocks to 1,460 in the loop workload, 18%
off the whole workload.

The lesson is the one #48 wrote down for itself and we didn't follow: a static
read of type sizes tells you what is *big*, not what is *hot*. Eight of the eight
landed items were real improvements to frame size — the per-recursion-level stack
did drop from ~380 KB to ~55 KB — but not one of them was in the top three
allocation sites of any workload we measured. The profile costs an afternoon and
would have redirected the whole effort.

---

## The burndown: sixteen issues in one orchestrated day (2026-07-17)

Amy asked for a backlog burndown with a specific shape: one orchestrator
managing GitHub issues and merges, Sonnet subagents doing the coding toil in
isolated worktrees, and kaibo providing outside review on every PR before it
landed. Seventeen PRs merged (#207–#211, #214, #215, #219, #221, #224, #226,
#227, #230–#232, #234) closing sixteen issues (#144, #147–#149, #154, #164,
#170, #176, #177, #183, #188, #189, #191, #217, #218, plus the #224 fix-forward)
— each with its own devlog/CHANGELOG story; this entry is about what the
*orchestration* taught us.

**Waves beat a queue.** Work went out in three waves sorted by diff footprint:
small independent fixes first (parallel), wide-ripple changes (#170's test-gate
sweep, #164's `to_argv` Result) only after the small ones merged, and
sequenced pairs where one PR would otherwise land twice (#189's binder polish
deliberately waited for #188's twin removal so it patched ONE binder, not two).
CHANGELOG both-added conflicts were the only recurring merge friction —
resolved mechanically by merge-main-in, keep both bullets.

**"Worse than filed" was the day's refrain.** Three issues deepened on TDD
contact: #144's repro was unfixable by the filed parser arms alone (the lexer
was swallowing the case-arm's closing paren into the bare-word token); #217's
"multi-occurrence" silent drop turned out to cover single occurrences too
(`env -u $BIN` had NO loud path at all); #183's arithmetic-swallow had a third,
unfiled instance in the primary async interpolation path — plain
`echo "$((1/0))"` printed silently at exit 0. Briefing agents to verify every
issue claim against current code before fixing is what surfaced all three;
that brief stays.

**The review pair earned its keep on the one PR where it mattered.** #230
(bytes-typed stdin, BREAKING) had a clean deepseek diff review; the
second-family holistic pass (or-kimi, after gemini 503'd and or-gpt's cast
turned out broken) found a real regression the first review missed: the stdin
gate move newly exposed `xxd -r`'s pre-existing lossy decode to binary input —
silent corruption where the old redirect-time gate had been loud. Single
reviews stayed fine for narrow PRs; breaking/wide PRs get a pair.

**Reviews as issue factories.** Beyond the fixes themselves, kaibo reviews
filed six follow-ups: #212 (background/scatter exit-3 remap gap), #222
(scatter|gather pipeline bypasses finalize_output), #223 (jq --arg stringifies
binary envelopes), #228 (/proc//etc read-only positioning, narrowed out of
#177 so the closed issue stays accurate), #229 (dispatch twin's missing
exit-126 checks), #233 (printf/patch lossy decodes + binary exit-code
consistency). The backlog shrank by sixteen and grew by six — all six smaller
and sharper than what they replaced.

**Tooling lessons** (also in auto-memory): `gh pr checks --watch` and naive
poll loops both race a fresh push — the previous head's completed checks
read as a false ALL-PASS; wait for the new head's checks to reach *pending*
before polling to terminal. And never read merge-conflict lists from
`git merge | tail` — CONFLICT lines scroll out of the window (bit twice);
`git status --short | grep '^UU'` is the authoritative list.

---

## Arg-binding polish: four small gaps, one shared core (2026-07-17, GH #189)

Sequenced right after #188/#231 merged the sync/async arg binders into one
`bind_tool_args` core specifically so this punch list would land once instead
of twice. Each of the four items was re-verified against the merged code
before touching anything, since the issue predated #215/#221/#224 too — two
of the four turned out to need a different fix than the issue's own text
suggested, and one uncovered a sharper bug than expected.

**Post-`--` `WordAssign`.** The `Arg::WordAssign` arm checked `accepts_word_assign`
(the export/alias/unalias allowlist) but never `past_double_dash` — only the
flag arms checked that. `export -- A=1` still bound `A=1` as a named
assignment. Chasing down whether this was actually *observable* (export and
alias both defensively read `args.named` **and** `args.positional`, so the
export/alias examples in the issue text turn out functionally harmless either
way) led to `unalias`, which is on the same allowlist for symmetry but never
reads `args.named` at all — it reconstructs argv via `to_argv()` and re-parses
with clap. A lingering named entry renders as a flag token (`--foo=bar`)
`UnaliasArgs` never declares, so `unalias -- foo=bar` crashed with clap's
"unexpected argument" error instead of treating `foo=bar` as a literal (if
odd) alias name to remove. Fix: `accepts_word_assign && !past_double_dash`.

**`--flag=true` flagify.** Confirmed live: `seq --json=true 1 3` exits 2
today. The issue's own suggested fix ("flagify once in the kernel before
dispatch") pointed at a post-hoc pass over the built `ToolArgs`, but that
would also catch `export A=true` (a `WordAssign`, not a `--flag=` token) and
silently turn a real assignment into a bare flag — a regression the pipeline
tests would never have caught since none exercise that shape. Scoping the
fix to the `Arg::Named` arm specifically (the only place `--flag=value`
syntax actually binds) avoids that: a `Value::Bool` lands in `flags`/gets
dropped only when the key isn't a declared *value-taking* param, mirroring
`ToolArgs::flagify_bool_named`'s logic without needing a schema object
(`param_lookup`, already assembled for the rest of the binder, has enough).
This also fixes `--json` itself — deliberately excluded from every builtin's
reflected schema (`clap_schema::is_skipped`), so no per-builtin
`flagify_bool_named` call ever normalized it; only the shared binder can.

**Short space-flag guard.** The long-flag ambiguity guard (`--type explorer`
under a `map_positionals` schema) was a straight port to the single-char
`ShortFlag` arm — same "fail loud, don't guess" logic, minus the `--flag=value`
escape-hatch suggestion in the error message, since kaish has no `-f=value`
form for short flags (confirmed: it's a parse error today, the no-pasting
guard's own turf).

**Pasting hints.** This one had a real landmine. Extending the no-pasting
guard from "adjacent `Positional`s" to "adjacent `Positional`s or `LongFlag`s"
(so `--flag$(echo x)` errors instead of silently splitting into a bare flag
plus a stray positional) seemed safe — until `cargo test --all` turned up
`cut_bare_comma_delimiter_glued` failing. `cut -d,` is `-d` (a `ShortFlag`)
glued to a bare `,` (a `Positional`, zero source-gap) — exactly the
getopt-style glued-short-value idiom (`cut -d,`, `head -c5`) the binder
already supports, and a comma isn't in the flag char class so it can't fuse
into one lexer token the way `-f1` does. `ShortFlag` stayed out of the glue
check entirely; `LongFlag` went in (there's no long-flag glued-value idiom to
protect). The redirect-target half (`> /tmp/$(echo x).txt`, previously a
generic chumsky "expected ..." error with zero quoting hint) needed its own
peek-and-reject wrapper around `redirect_parser`'s `target` parser — first
draft called a fresh `primary_expr_parser()` for the peek and blew the stack
immediately, because the module's own doc comment already warns why: `target`
is threaded in specifically so `$(cmd > file)` doesn't reconstruct
`redirect → primary_expr → cmd_subst → redirect` forever. Reusing the
caller's own `target.clone()` for the peek (chumsky's `rewind()`, no
consumption) fixed it — the grammar was already built once by the caller, so
cloning the *value* is cheap and doesn't re-enter construction.

Full `cargo test --all --locked` / `cargo clippy --all --all-targets` /
`cargo check -p kaish-kernel --no-default-features` clean throughout. GH #189.

---

## Collections residuals — bracket-path `push`, quoted `]`, record-value hint, loud arithmetic (landed 2026-07-17, GH #183)

Four independent items off the collections punch list, verified fresh against
current `main` before touching anything (the issue was written against an
earlier commit; all four were still live, and the arithmetic item turned out
worse than described).

- **Bracket-path `push` target ships** (`push services[web][tags] item`).
  The blocker was the lexer: `push`'s target has no trailing `=` to key off
  the way an assignment lvalue does, so `services[web][tags]` fused into a
  `GlobWord` and glob-expanded (loud "no matches") before `push` ever ran.
  Fix is a THIRD, fully independent tracker (`PushTarget`) alongside the
  existing assignment DFA in `compute_value_context` — deliberately not
  folded into it, so a variable literally named `push` (`push=5`,
  `push[0]=x`) keeps working untouched. It recognizes a bareword `push` at
  statement-head, then treats the immediately-following glued `[...]` run as
  a path to fuse verbatim into a single `Ident` (never a `GlobWord`).
  `Scope::walk_append` now takes a full `VarPath`: a bareword target still
  appends top-level; a bracket path walks intermediates with the same
  no-autoviv `resolve_step`/`descend_mut` `walk_write` uses, appending at the
  resolved leaf instead of replacing it. The old read/push/assign-back
  workaround in the docs is gone.
- **Quoted subscript keys can contain `]`** (`${r["weird]key"]}`).
  `parse_var_ref`'s bracket collector scanned to the *first* `]`, quote-blind,
  so an embedded `]` inside a quoted key ended the subscript early and
  produced a mangled, cryptic error. Now: if a subscript opens with a quote,
  consume verbatim to its OWN matching closer first, then resume the search
  for the real terminating `]`. Unquoted subscripts are untouched.
- **Unquoted multi-word record values get an actionable error.**
  `{msg: hello world}` was already a parse error (kaish's grammar makes a
  bare space-separated `key: value` pair without a comma legal —
  `{a: 1 b: 2}` — so "world" gets tried as a NEW key and fails at `}`
  expecting `:`), but the message was chumsky's generic "found '}' expected
  ':'". A `try_map` guard in `record_literal_parser`, run right after each
  entry's value, peeks (via `.rewind()`, consuming nothing) for exactly this
  shape — an `Ident` not itself followed by `:` — and raises a message that
  names the mistake and shows the quoted fix. `docs/LANGUAGE.md` already
  described this as "a parse error with the quoted fix in the message" before
  today; the code just didn't match the doc yet.
- **Sync arithmetic errors stopped being silently swallowed — and it was
  worse than the issue said.** The issue named `scheduler/pipeline.rs`'s
  reduced sync arg binder (`eval_simple_expr`'s bare `Expr::Arithmetic` fell
  into the generic "not representable here" `Ok(None)`, silently dropping
  the flag entirely — even a *valid* `scatter --limit $((1+1))` never
  bound; `eval_string_parts_sync`'s `StringPart::Arithmetic` used
  `if let Ok(..)`, silently splicing in nothing on error). Both fixed with
  `?`-propagation, matching the message convention (`"arithmetic error: {e}"`)
  the two already-loud sites use. But testing turned up a THIRD, more severe
  instance in the exact same shape sitting in the PRIMARY async
  string-interpolation path (`kernel.rs`'s `eval_string_part_async`) —
  `echo "value: $((1/0))"` printed `value: ` at exit 0, no error at all, in
  completely ordinary command execution, not just the scatter/gather corner
  the issue scoped. Left un-fixed that would have been an inconsistent,
  worse-in-practice sibling of the very bug this item exists to kill, so it
  got the same treatment. All three now match the sync interpreter's
  (`eval.rs`) arm, which already propagated correctly and was never part of
  the swallow.

Kernel-routed tests throughout: `collections_lvalue_tests.rs` (bracket-path
push, including a `push=5` regression guard that the new lexer tracker
doesn't touch plain variable assignment), `lexer_tests.rs` (token-level
fusion pins for the `push` target and the quote-aware bracket collector),
`collection_access_tests.rs` (quoted-key-with-embedded-bracket reads),
`collection_literals_tests.rs` (strengthened the existing loud-error test to
check the actual message, added a comma-optional-entries regression guard),
`correctness_oneoffs_tests.rs` (interpolated arithmetic), and
`scatter_gather_jsonl_tests.rs` (the scatter-flag-value arithmetic cases,
paired loud/happy-path per the file's existing convention).

---

## Eliminating the sync `build_tool_args` twin (2026-07-17, GH #188)

The scheduler had a second, hand-rolled arg binder living next to the real
one: `Kernel::build_args_async` (kernel.rs) is the production flag/positional
binder — schema-aware, subcommand routing (`select_leaf`), glued short-flag
values, `consumes`/`repeatable` accumulation, real glob/tilde expansion,
command substitution via full async recursion. `scheduler::pipeline::build_tool_args`
was a *reduced* sync twin of the same logic, needed because scatter/gather's
own option parsing (`--as`, `--limit`, `--timeout`) has to bind before any
worker forks — too early to recurse back into the async pipeline — and the
`#[cfg(test)]` `BackendDispatcher` used it too, so pipeline/runner unit tests
could exercise schema-aware binding without spinning up a full `Kernel`. Two
implementations of the same flag-binding rules is exactly the drift class GH
#133 named for the external-command spawn sites; this was its sibling in the
arg-binding seam, and the code already carried three comments admitting the
gap (no undeclared-space-flag guard, no glued short-flag handling, no
`consumes`/`repeatable` accumulation) — all "currently un-triggerable" only
because scatter/gather's own schema happens to be scalar-only.

The fix extracts the structural binding logic — everything except "how do I
evaluate one expression" — into a single `bind_tool_args` core (kernel.rs),
parameterized over a new `ArgValueSource` trait (`eval`/`expand_glob`/`home`).
`Kernel` implements it with its real capabilities; `build_args_async` is now
a one-line call into the shared core. The reduced sync path gets its own
`SyncEvalSource`, wrapping the same `eval_simple_expr` it always used (still
no command substitution — that capability boundary is real, not laziness),
and `scheduler::pipeline::build_tool_args` becomes a thin `async fn` wrapper
around the shared core instead of a parallel implementation. Both the
scatter/gather call site and the test-only `BackendDispatcher` converge on
it unchanged in call shape (still `Result<ToolArgs, String>`), so the ~30
direct unit tests of the old twin only needed a mechanical `#[test]` →
`#[tokio::test]` conversion, not a rewrite. One test (`test_unknown_flag_in_schema`)
turned out to be pinning the exact undeclared-space-flag bug the issue named —
split into a loud-error pin and a separate unambiguous-case test, plus new
tests proving glued short-flag values and `repeatable`/`consumes>1`
accumulation now bind correctly through the reduced path too. No CHANGELOG
bullet: scatter/gather's real schema is still scalar-only, so nothing
user-observable changed today — the fix is that the next flag which needs
any of this can't silently misbind through the reduced path anymore.

---

## The lexer becomes one machine (2026-07-16)

GH #95 sat locked since 2026-07-04: two cross-model batch reviews (gemini-pro,
fable) had independently concluded that `lexer.rs`'s four-stage pipeline — two
string-rewriting preprocessors → logos → marker re-threading → three fusion
passes — was architecturally overdue, its stages communicating through in-band
marker strings, span adjacency, and pass ordering that nothing checked. Amy's
call was aggressive: full rewrite, single PR, trust the ~4,100-test suite as
the spec. It held up — the entire workspace suite passed the new engine after
exactly one fix (bare-CR heredoc terminators, a Mac-classic edge the old code
normalized as a side effect of its rewriting).

The census phase did the heavy lifting. Tracing the seams before writing code
settled the two questions #95 left open and overturned one of its assumptions:
multiline list/record literals are legal (the parser eats interior newlines),
so the context stack must NOT reset at newline inside an open literal — and
nested `$((..))` inside `$( )` was never "the subcommand's problem" because
substitution bodies are lexed inline; the old skip just left inner arithmetic
raw and unparseable. The census also found two corruption bugs #95 didn't
list: `${X:-$((1+2))}` leaked raw `__KAISH_ARITH_…__` marker text into the
AST, and the `#` of `$#` opened comment state in the preprocessor. And it
found that the grammar accepts *spaced* assignment (`x = [a b]`), which
reshaped the bug-5 fix from "require span adjacency" to a small statement-head
DFA that models the real assignment shapes (glued, spaced, `local`,
subscripted lvalues, env-prefix chains) — an argv `=` opens nothing.

The engine: one composed scanner with explicit quote/escape/comment state
extracts heredocs and arithmetic together and records a complete replacement
table in both coordinate systems — heredocs included, which kills the span
drift the old pipeline documented in its own comments (`body_start_offset` is
now exact). Markers resolve back to tokens positionally, keyed by table
ranges; a word glued onto a marker splits into `Arithmetic` plus re-lexed
fragments so the parser's no-pasting guard fires loudly where marker garbage
used to leak (`echo $((1+2))abc`; bash's `3abc` interpolation stays rejected
as scope creep, but `echo "$((1+2))abc"` works). Fusion slices its text
verbatim from the source, so `a:007` and `007*` finally keep their zeros. The
context pass is an explicit frame stack — `$( )` pushes a fresh scope, which
is what fixes `[[ -n $(x=[a]) ]]` leaking test depth into the substitution.
The logos vocabulary and `Token` enum were not touched: ~60 shipped idiom
decisions live in those regexes and none of the bugs did.

One deviation from #95's locked sketch: the three fusion passes stayed three
passes (sharing the one new context walker) instead of collapsing into one.
Their alphabets differ deliberately — `Float` colon-fuses but doesn't
glob-fuse (`1.5:x*` fuses; `1.5*` stays split) — and a union-alphabet single
pass would have changed tokenization in corners the suite pins. Composition
preserved the interactions; the bugs weren't in the pass structure anyway.

Review pair per house style, pointed at the worktree: the deepseek consult's
one HIGH finding (line continuations corrupting heredoc collection) didn't
reproduce — the scanner's behavior is bash's (continuation joins the
introducer line; `cat <<EOF \` + `| tr a-z A-Z` uppercases the body),
verified end-to-end and pinned as a test — but its four smaller cleanups were
real and landed. The gemini-pro batch leg came back truncated mid-thought;
its one visible finding was the documented spaced-lvalue trade-off. 25 new
characterization tests in `lexer_pipeline_tests.rs` keep all eight corruption
classes dead.

---

## kaish gets CI, and CI immediately earns its keep (2026-07-13)

Three years of shell projects say the first CI run never passes; kaish kept the
streak. PR #169 added the repo's first GitHub Actions workflow — modeled on
kaibo's (pinned-SHA actions, minimal permissions, per-ref concurrency) but
encoding kaish's own CLAUDE.md gates: `cargo test --all --locked` + clippy
`-D warnings` with `--all-targets`, the kernel's no-default-features leg, the
`kaish-wasi` wasm32-wasip1 build, and a tripwire for committed `.snap.new`
files. No release workflow on purpose: kaish ships crates, publishing stays the
manual `/release` runbook, and kaibo's TLS-invariant guard didn't come along
because kaish has no TLS surface.

The first live run failed two of three jobs, and every failure was a
pre-existing repo bug, not a workflow bug:

- `non_interactive_stdin_is_dev_null` had been silently testing the wrong
  process for who-knows-how-long. Its comment said "/bin/readlink to bypass the
  builtin"; its code ran bare `readlink` — the kaish builtin — which resolves
  the *test process's* fd/0. It only ever passed where the test runner itself
  had stdin=/dev/null; GitHub's runner hands the step a pipe. Reproduced red
  locally with `echo poke | cargo test …`, fixed by making code match comment.
- The no-default-features leg was written as `cargo test`, which has **never
  compiled** — `KernelConfig::repl()` is localfs-gated and ~25 test files call
  it. The embarrassing part: local "verification" had piped cargo through
  `tail`, reading tail's exit code. The documented invariant is "sandbox-mode
  *compiles*", so the leg became `cargo check`; the test upgrade is GH #170.
- Run two found a third class: CI's stable (1.97) out-lints local (1.96) —
  `clippy::question_mark` got smarter and flagged a strip_prefix match in
  ignore_config.rs. Fixed the code, didn't pin the toolchain; catching
  toolchain drift is a feature, not a flake.

A kaibo (deepseek) review of the workflow pre-push contributed the fourth fix:
`.gitignore` had no `*.snap.new` entry, so the exact mistake the CI tripwire
catches could still be staged locally by `git add .`.

The doc sweep that followed (this PR) wired CI into the contributor story:
README badge + gates in "Building from Source"/"Contributing" (and the stale
"Status: 0.11" became badge-driven instead of hand-maintained), CLAUDE.md's
gates section now names ci.yml as the enforcement point and warns about
runner-vs-local clippy drift.

---

## The REPL learns to read .gitignore — and kaish-ignore learns to persist (2026-07-06)

Amy's call on #134: default to ignore-aware. The reference REPL had always run
`IgnoreConfig::none()` — zero filtering — so every interactive `glob '**/*'`
or `grep -r` walked `target/`, `.git/`, and `node_modules/` in full (it was
half of the "glob walked forever" repro behind #122). The new
`IgnoreConfig::interactive()` preset is `agent()`'s filters at **Advisory**
scope: `.gitignore` plus the default ignore list for the polite walkers,
`find` still POSIX-unrestricted, and two escape hatches — `--no-ignore` per
call, `kaish-ignore clear` per session. Bare embedded kernels keep `none()`;
the choice is per-frontend, not global.

The trap discovered while testing the opt-out: `kaish-ignore clear` printed a
cleared config and then the very next statement saw the old one. The
per-command context sync in `execute_command` copied back cwd, aliases, and
output-limit — with a comment documenting exactly this bug class for
output-limit — but never `ignore_config`, so every runtime ignore mutation
died at the end of its own statement. The documented `.kaishrc` recipe
(`kaish-ignore add .gitignore`) had been a no-op the whole time. One line in
the sync closes it, and the missing-field-in-a-manual-sync class gets another
tally mark for the "extract the ctx-sync helper" backlog entry.

---

## The latch survives its consumers (2026-07-06)

A pre-release "fishing expedition" — Amy's cross-model combo, a gemini-pro
batch over the whole scheduler/jobs/REPL surface as whole files plus a
deepseek consult over the same waters, no diff so the models read the code
cold — converged independently on the same finding: the backgrounded latch
that #96 had just made *reachable* was still *destructible*, silently, by
both of its housekeeping consumers. `jobs --cleanup` reaped a latched job
because `is_done()` counts Latched as done; `kill %N` ran cancel+remove
unconditionally. Both verified against the binary before believing the
models: "Cleaned up 1 completed job(s)", gate gone, the destructive op
permanently unconfirmable. The stored LatchRequest is the *only* handle to a
backgrounded gate — dropping the job drops the contract.

The decisions: cleanup keeps latched jobs and says so ("Kept 1 latched
job(s)…"), because a silently-retained job is just a differently-shaped
surprise. `kill %N` refuses with a pointer to `/v/jobs/N/latch` — kill is
not repurposed as the discard path, since fat-fingering kill at a gate you
meant to confirm was precisely the found failure mode. The explicit path is
`kill --discard %N`: loud about what it abandoned, conflicts with `--signal`
at the clap layer (discarding delivers nothing to anyone). Review (deepseek,
worktree checkout) mapped every other job-dropping path — fg/bg structurally
can't reach a latched job (no pid/pgid, not stopped), shutdown waits but
never removes — and flagged the one unguarded seam, `JobManager::remove`,
now documented as latch-bypassing with `cleanup()` named as the safe bulk
path. Rider from the same sweep: `Job::try_poll`'s "shouldn't happen"
Pending branch had already taken the JoinHandle and would have dropped it,
stranding the job as Running forever with its result silently lost; it puts
the handle back now.

The latch-visibility residuals the sweep also surfaced (wait --json renders
`"[1] Latched\n"` with no nonce; jobs --json rows omit the latch; scatter
rows can't carry one; mid-pipeline gates lose their exit code) were filed as
#124/#125 rather than stretched into this PR.
## `--include` learns to filter; the family goes loud (2026-07-06)

The same fishing sweep grep-trawled for siblings of #122's bug classes —
flags read off the raw ToolArgs map instead of the parsed clap struct,
`Option<String>` numerics with `parse().ok()` fallbacks — and the walk-filter
family turned out worse than the deferred nits suggested. The headline:
`glob --include` had never filtered anything. `IncludeExclude` pushed Include
rules into its list, but `should_exclude` only ever acted on Exclude matches;
the "strict mode" its own doc comment promised was never written, and the
walker only asked the exclude question. grep's `--include` merely *looked*
functional because it separately baked the single include into its walk
pattern as `**/{inc}`. On top of that, repeating `--include`/`--exclude`
silently kept only the last value (glob's help said "can be repeated"), so
`grep -r TODO . --include='*.rs' --include='*.toml'` answered with a silent
false negative — the worst shape of wrong for an agent. And the numeric
cousins all failed toward danger: `--depth=abc`/`-L abc`/`-maxdepth xyz`
walked unlimited at exit 0, `spawn timeout=abc` silently *disabled* the
timeout.

The include semantics decision: rg-like. When include rules exist a file must
match one — checked against the relative path, then the basename, first
Include/Exclude verdict wins — but a directory is never excluded by
include-miss, so traversal still reaches included files below it; excludes
keep pruning subtrees. That distinction (files strict, dirs traversable) is
the part the old two-call walker check could never express: the basename call
would have rescued `src/lib.rs` against `*.rs` only *after* the relative-path
call had already dropped it. One entry-aware `excludes_entry()` replaced the
pair, `should_exclude` was deleted rather than kept as a shim, and grep's
pattern hack went with it. Repeatables became `Vec<String>` +
`read_repeatable_strings` — the ftype pattern that was ten lines away all
along — and every numeric in the family now refuses bad and negative values
loudly. Eleven kernel-routed tests pin it, including the
include-doesn't-block-recursion case.

---

## `glob **/*.rs` stops eating its own pattern (2026-07-06)

Amy hit it live at the REPL: `glob **/*.rs` at the kaish repo root took a long
time, then printed exactly one file — `crates/kaish-client/src/embedded.rs`.
Contributing factors, in order: (1) an unquoted pattern in argv position is a
`GlobPattern` token, and the kernel's argv binder pre-expands those into
matching paths before the tool runs — correct shell semantics for `cat *.rs`,
fatal for a tool whose *input is the pattern*; (2) the `glob` builtin read
positional 0 as its pattern and silently dropped the rest, so the first
pre-expanded path (alphabetically first — kaish-client sorts first) became the
"pattern", an all-literal glob that matches exactly itself; (3) the REPL runs
`IgnoreConfig::none()`, so the bind-time walk descended all of `target/`
unfiltered — that was the "long time", paid twice because the builtin then
walked again to match the literal path. The builtin's own schema examples
teach the unquoted spelling, so every agent following the help walks into it.

The fix is a first-class seam, not a special case: `ToolSchema` grows a
`glob_passthrough` flag (sibling of `raw_argv`/`owns_output`) telling the
binder to hand bare patterns through as written; the eval fallback already
binds `Expr::GlobPattern` to its literal text (it's what `set +o glob` used).
`glob` opts in and now consumes *all* positionals as patterns — deduped union
in pattern order, strict per-pattern no-match errors that name the missing
pattern — instead of silently ignoring everything past the first. This is
also consistent with the builtin-side expansion family: cat/head/tail/ls/…
already do their own glob eval on string args via `ctx.expand_paths`, so the
tool owning pattern semantics was the established pattern; the binder just
had to stop pre-chewing glob's input. Embedder tools with pattern-shaped
inputs get the same opt-in. The REPL's unfiltered `target/` walk (slow even
when correct) stays open as a follow-up.

## Escaped quotes in `${VAR:-default}` (2026-07-06, GH #93 item 5)

Item 5 of #93's punch list: `unquote_default_word` (the function that strips
the syntactic quotes off a `${VAR:-"default"}` word before it's parsed for
interpolation) toggled its `in_single`/`in_double` state on *every* `"`/`'` it
saw, with no notion of a preceding backslash. So `${UNSET:-"hello
\"world\""}` — a double-quoted default containing an escaped inner quote —
had its second `"` prematurely close the quoted region, and the value came
out as `hello \world\` instead of `hello "world"`.

The fix needed to answer a harder question than "skip escaped quotes": what
does a *run* of backslashes immediately before a quote mean? Bash's actual
rule (verified empirically against real bash, not from memory) pairs
backslashes left-to-right — an even run collapses to half as many literal
backslashes and leaves the quote as a real, state-toggling delimiter; an odd
run does the same collapse and additionally escapes the quote (literal
character, no toggle). A naive one-token lookahead gets this wrong on 2+
backslash runs (`"a\\"` → `a\` in real bash; a lookahead that treats each
backslash independently would misjudge the second backslash as escaping the
closing quote instead of pairing with the first). The fix buffers the
contiguous backslash run and decides once it hits the terminating character.

Backslashes *not* adjacent to a quote were left untouched on purpose — real
bash also collapses `\\` inside double quotes when it's followed by an
ordinary character (verified: `"foo\\bar"` → `foo\bar` in bash), but
`unquote_default_word` never did general backslash-escape processing before
this fix (confirmed by reading `parse_interpolated_string`, its downstream
consumer, which has no backslash handling at all), and this PR is scoped to
the reported quote-toggle bug, not a full escape-processing rewrite. Recorded
as a known gap in the PR body rather than filed as a separate issue — it's a
narrow, pre-existing limitation, not a regression.

The single-quote case forced a judgment call, and the *first* answer was
wrong. The initial pass extended the same backslash-escape rule *into*
single-quoted default words "for symmetry," reasoning that real bash's
behavior there (`'hello \'world\''` is a syntax error — unterminated quote —
not an embedded apostrophe) wasn't a coherent target to match. Amy overruled
it on review, and correctly: single quotes are a *literal* region in shell,
full stop. A model relying on shell muscle memory must get **zero** surprises
inside `'…'` — zero interpolation, zero escape processing. A backslash there
is a literal byte and a `'` always closes the span; it is never escaped. The
"symmetry" argument was inventing a dialect where the whole point is fidelity
to shell's literal-region contract.

So the escape logic is gated to fire only *outside* single quotes (`if ch ==
'\\' && !in_single`). Nothing is actually lost: the shell-correct way to embed
a single quote is the `'…'\''…'` idiom — close the span, emit an **unquoted**
escaped `\'`, reopen — and that unquoted escape still works (it's the same
code path as the double-quote fix, just outside any quote). `${X:-'it'\''s'}`
→ `it's`, matching bash exactly. The one behavior that stays inside single
quotes is the pre-existing `$` → `__KAISH_ESCAPED_DOLLAR__` marking, which is
what *implements* "zero interpolation" — it isn't escape processing, it's
suppression.

The delimiter-stripping itself (`${X:-'x'}` → `x`) was double-checked and is
correct — that's the function's whole purpose, the quotes are syntax not data.
Only the escape-processing-inside-single-quotes overreach was the bug.

A third round, from a kaibo review of the revision, caught a subtler
context bug: the escape predicate treated `'` and `"` identically, gated only
on `!in_single`. So inside a *double*-quoted region a `\'` still escaped —
`${X:-"a\'b"}` came out `a'b`. Bash disagrees: inside `"…"` a `'` is an
ordinary character, and only `\"`, `\$`, `\\`, `` \` `` are escapes, so the
backslash before `'` is literal and bash yields `a\'b` (verified against real
bash). The fix is a one-line narrowing of the predicate — a `'` only counts as
escapable when *not* `in_double` — so unquoted `\'` still powers the
`'it'\''s'` idiom, double-quoted `\'` stays literal, and single-quoted regions
(already fully literal) are untouched.

Tests in `shell_compat_tests.rs`: the double-quote fix is pinned by
`default_word_double_quoted_escaped_quotes_literal`,
`default_word_escaped_backslash_before_quote` (the `"a\\"` → `a\` parity
case), `default_word_mixed_single_and_escaped_double_quotes`, and
`default_word_double_quoted_backslash_before_squote_literal` (the kaibo-caught
`"a\'b"` → `a\'b` case, confirmed red as `a'b` before the predicate fix). The
shell-literal single-quote contract is pinned by four cases:
`…_strips_delimiters` (`'x'` → `x`), `…_no_interpolation` (`'$HOME'` →
`$HOME`), `…_backslash_literal` (`'a\b'` → `a\b`, backslash NOT collapsed),
and `…_embed_idiom` (`'it'\''s'` → `it's`). All the newly-fixed cases were
confirmed failing against the pre-fix code; every case — single- and
double-quoted alike — now passes under `KAISH_BASH_COMPAT=1` against real
bash, with no recorded divergence, because the shell rule *is* bash's rule.

## Binary at the remaining text sinks, and `&>` streaming (2026-07-06, GH #93)

0.11.0 made `Value::Bytes` go loud at the three primary text sinks — string
interpolation, bare-word external argv, `echo` — via `value_to_text_sink()`.
#93 item 1 asked for the rest of the sinks that still fell through to
`value_to_string`'s infallible `[binary: N bytes]` placeholder. Rather than a
bespoke fix per sink, added `value_to_text_sink_named(value, sink)` (and a
`values_to_text_sink_named` for a whole positional list) — same guard,
parameterized so the error names the actual boundary ("a path", "an exported
environment variable value", "a redirect target") instead of the generic
"text", mirroring the `sink` parameter `structured_boundary_error` already
uses for the collection-vs-process-boundary guard.

**The five named sinks, and one the sweep turned up.** Builtin
path-positional coercion touched ~17 files (`mkdir`/`cp`/`mv`/`rm`/`touch`/
`dirname`/`cut`/`stat`/`readlink`/`realpath`/`tee`/`sort`/`find`/`grep`/`sed
-i`/`ls`) — all the same shape, so `cp`/`mv`/`tee` also got a small cleanup:
they were stringifying the same source value twice (once for a
gate-overwrite preview, again in the write loop) — computed once now. Widened
"path-positional" to cover `[[ -f $x ]]`/`test -f $x` too (kernel.rs's
`eval_test_async` and the `test` builtin's own separate file-test impl,
matching in comments) since a binary operand there silently stats a file
literally named `[binary: N bytes]` — same bug, different entry point.
Env-var export needed three call sites in sync (kernel.rs's production spawn,
its `dispatch.rs` test-only twin, and `env`'s own `execute_with_env`), all
already following the collection guard's precedent of a separate binary
check after `structured_export_error`. The redirect target
(`pipeline.rs::eval_redirect_target`) had a private, single-caller
`value_to_string` shim living in the same file — deleted once its call site
converted, rather than leaving it as dead code. Sweeping the codebase for the
"bare-word external argv" pattern (already fixed in `build_args_flat`) turned
up a fourth spawn site nobody had touched: `exec`'s own argv loop, built
straight from typed positionals with no shared helper — same class of bug,
now fixed the same way.

**The semantic ops split three ways on inspection**, not the four the issue
guessed. `${#…}` on binary was *already* correct (`value_length` special-cases
`Bytes` to the byte count before ever reaching `value_to_string` — locked in
with a regression test, no fix needed). `==`/`!=` needed a new guard arm in
`values_equal` ahead of the generic mixed-scalar fallback: `Value::Bytes`
against anything but another `Value::Bytes` is now a loud type error, not a
silent compare-the-placeholder-text. `in` needed the same treatment only for
the record-key branch of `eval_membership` (a `Value::Bytes` needle stringified
into a lookup key); the list-membership branch (`element_matches`) keeps
treating a shape mismatch as "not a match, not an abort" — consistent with how
it already treats a nested-collection element, and it's what keeps `x in
$heterogeneous_list` from dying partway through the scan. `case`-glob needed
one line in `kernel.rs`'s `Stmt::Case` handler.

**Heredoc body and record-key interpolation turned out to be a non-finding
worth writing down anyway.** The sync `Evaluator`'s `HereDocBody` and
`RecordKey::Interpolated` arms (`interpreter/eval.rs`) are the ones the issue
named, but tracing every real call site of the sync `eval_expr` showed
heredocs and record literals in an actual script always resolve through the
kernel's *async* evaluator, which already composes through the guarded
`eval_string_part_async` — the sync arms are unreachable with a live
`Value::Bytes` in production (only reachable by an embedder driving the sync
`Evaluator` directly, or by a unit test). Swapped `value_to_string` for
`value_to_text_sink` there anyway — cheap, correct, and stops the code from
leaning on a non-local invariant — but it's a defense-in-depth edit with no
behavior change, not a bug fix; said so plainly rather than overclaiming a
fourth "fixed" sink. Confirmed by literally reverting just those two lines
and rerunning the new eval.rs unit tests: both still passed.

**A test-helper trap almost hid a false pass.** The shared `assert_loud_binary`
helper (from 0.11.0) checked `err.contains("binary")` — true of a real
"cannot be used as a path" message, but *also* true of `rm`'s ordinary "No such
file or directory" once the path itself is the literal string `[binary: N
bytes]` (the placeholder text contains the word "binary"). `rm`'s new test
silently passed against the *unfixed* code for exactly that reason. Caught it
by deliberately reverting the whole PR's `src/` diff (keeping the new tests)
and rerunning — 11 of the 21 new/touched tests failed as expected, but `rm`'s
didn't. Tightened the helper to `contains("cannot be used as")` — the
consistent wording all these guards now share (reworded `values_equal`'s
message to fit) — and added a stderr placeholder-leak check alongside the
existing stdout one. Reran the full revert to confirm: all 11 fail pre-fix,
all pass post-fix.

**Item 6 (`&>` streaming) is an honest equivalence test, not a failing one.**
`RedirectKind::Both` now uses `take_output_for_stream`/`write_canonical`
like `>`/`>>`, instead of forcing structured output through a full
`to_canonical_string()` `String` first — a memory-copy optimization, not an
output-correctness fix, so the file bytes are identical either way and a
test can't observe the difference by content alone. Wrote it up front as
that: byte-for-byte equivalence + a large-table completeness check, in
`pipeline.rs`'s own `apply_redirects` test module (unit-level, not
`kernel.execute()` — there's no builtin under test, just the redirect
machinery, and the existing merge-redirect tests already use this pattern).
Verified honestly by reverting just that hunk: same tests, same pass. Applied
the identical streaming swap to the inter-stage pipe write next to it
(`run_pipeline`) since it shares the exact `out_bytes()`/
`text_out().into_owned().into_bytes()` shape — not `&>`-specific, but the
same anti-pattern sitting right next to the one the issue named.

**Deferred, tracked in #116, not fixed here:** a `WordAssign`→positional/named
reconstruction cluster (`dd if=$BIN`, `awk -v a=$BIN`, `cat foo=$BIN`) across
four call sites in `kernel.rs`/`pipeline.rs` that embeds a value into a
`key=value` string the same lossy way — real, but a distinct code shape from
the five named sinks, and out of scope for one PR. Also verified in passing
that #93's two SUSPECTED findings (S3/S4: binary crossing into `fromjson`/
`fromjsonl`) are already handled — both already reject a binary positional
loudly and route stdin through the shared `read_stdin_to_text`, which already
errors on invalid UTF-8.

**A kaibo review of the diff (deepseek, whole-file, not just the patch) found
a second wave the grep-for-`value_to_string` sweep missed entirely**, because
it's a different anti-pattern: `ExecContext::expand_paths` (the shared path
list builder behind `cat`/`head`/`tail`/`wc`/`checksum`/`file`/`base64_tool`/
`tac`/`xxd`) matched non-string positionals with `_ => continue` — a
`Value::Bytes` operand didn't get stringified into a placeholder, it just
*vanished* from the list. Worse than the placeholder bug: every one of those
callers falls back to reading stdin when the path list comes back empty, so
`head $BIN` (say) silently read whatever was piped in instead of erroring on
the binary path — confirmed with a test that pipes distinguishable stdin
content through and asserts it never appears in the output. Same shape at
`cd` (`get_string`'s silent `None` → falls back to `$HOME`) and `awk`'s input
operand (→ falls back to stdin); `basename`/`diff` already failed loudly on
`None`, just with a generic "missing" message, fixed for consistency.
`get_string` itself lives in the `kaish-types` leaf crate with no
`EvalError` machinery to explain *why* it saw nothing, so the fix is a
`get_path_string` helper in `tools/builtin/mod.rs` that checks for
`Value::Bytes` before falling through to `get_string`, used at each of the
four call sites — not a change to `get_string`'s own signature, which is used
far too broadly (for non-path args too) to safely touch here. Same
revert-and-confirm discipline as the rest of this PR: all 7 new tests fail
against the pre-fix code, pass after.

**A second kaibo pass over the same `get_path_string`/`expand_paths` fix
found 10 more builtins with the identical `get_string` shape** — `sed`
(streaming-mode input) and `uniq` silently fell back to stdin exactly like
`awk`; `jq`'s `path` positional the same; `tree` silently used `.`; `write`/
`ln`/`patch`/`validate`/`checksum`/`spawn` already failed loudly on `None`
(just with a generic "missing" message), converted for consistency. All
mechanical once `get_path_string` existed: swap `args.get_string` for it, add
the `Err` arm.

**Two of those ten (`checksum --check=$BIN`, `patch --file=$BIN`) exposed a
second, deeper bug while writing their tests — the tests kept passing against
unfixed code, for the wrong reason.** Both builtins check their own
clap-parsed field (`parsed.check`/`parsed.file`) *before* falling back to
`args.get_string`, and clap's field is populated from `ToolArgs::to_argv()` —
which stringifies `Value::Bytes` into the *exact same* `[binary: N bytes]`
placeholder via its own `value_to_argv_token`, a separate, pre-existing,
explicitly-commented-as-deferred gap in the `kaish-types` leaf crate. So
`parsed.check` was never `None` for a binary `--check=$BIN` — it was
`Some("[binary: N bytes]")`, and the `get_path_string` fallback (only reached
on `None`) never ran. Fixed by reordering: check the untouched `ToolArgs`
value first (via `get_path_string`), fall back to the clap field only when
genuinely absent — correct for both the binary case and the ordinary one
(the raw `ToolArgs` map already has whatever clap would have parsed, from
before `to_argv()` ever ran). Audited the rest of the `parsed.foo.clone()
.or_else(|| args.get_string(...))` sites in the codebase for the same
ordering hazard — none of the others (`algo`, `template`, `encoding`,
`separator`, `field_separator`, …) are path-typed, so out of *this* PR's
reach, but the root cause (`to_argv()`/`value_to_argv_token`) is filed as
#120 rather than silently left for the next person to rediscover the hard
way. Every genuinely-fixed builtin (11 new tests this round) verified
fail-first the same way as the rest of the PR.

**A third pass (gemini, asked specifically to sweep for the same ordering
hazard in every builtin touched so far) found one more: `cmp.rs`** read its
two file operands off `parsed.paths` (the clap-parsed field) instead of
`args.positional` — the exact CLAUDE.md gotcha ("read `Value`-typed
positionals off `args.positional`, not the clap struct") that the other
path-positional builtins already followed correctly. Fixed the same way as
`mkdir`/`cp`/etc.: `values_to_text_sink_named(&args.positional, "a path")`.
Three review passes, three distinct bug shapes (`_ => continue` drop,
clap-field-checked-before-raw-value ordering, clap-field-read-directly) —
each caught because the review asked "is this ACTUALLY the shape you think
it is" against the live code rather than trusting the pattern to have been
applied uniformly. Stopped the loop here rather than keep sweeping
indefinitely; #116/#120 carry the remaining known-adjacent gaps forward.

## The README learns to face the front door (2026-07-06)

Amy asked for a first-time-visitor evaluation of README.md ahead of a rewrite.
Two stateless outside reads (deepseek and gemini-flash via kaibo `oneshot`,
given only the README — exactly what a visitor sees) converged with our own
read almost point-for-point: the project thesis ("Why a shell for agents?")
was buried as an `####` heading inside Components → Embedding at 85% document
depth; the doc oscillated between agent-runtime and daily-driver-shell
identities section by section; and reference-grade material (exit-code table,
latch/trash mechanics, embedder lifecycle) clogged the middle. The accuracy
pass found worse: the very first instruction was broken — `cargo install
kaish` names a crate that doesn't exist (the binary ships in `kaish-repl`) —
and "still experimental" contradicted the settled-language stance.

Decisions:

- **Sequence, don't interleave.** The rewrite front-loads the universal pitch
  (hero with the "skills transfer" line both reviewers independently named the
  strongest sentence in the file, thesis, differences-from-bash), keeps the
  Quick Tour, then branches Getting Started into the two real journeys: REPL
  and embedding. Six crates ship this README as their crates.io page, so the
  embedder content stays — sequenced, not braided.
- **Reference material moved to its homes.** Exit-code table + output contract
  + fresh-kernel-per-request lifecycle → EMBEDDING.md (new "result contract"
  section; its stale "0.8.x" fixed too). Trash thresholds/exclusions →
  LANGUAGE.md — they had no home outside the README.
- **Everything shown is verified.** Every tour example ran against the current
  binary (including the quote-to-join parse error); the embed example was
  compiled and run from a scratch crate against the worktree kernel. The tour
  now shows 0.11 typed collections (`fromjson` + `${r[k]}`) instead of jq —
  Amy may spin jq out to its own crate, so the README stops showcasing it
  (it stays in the builtin inventory table, which tracks code).

## `jq -s` stops being a no-op on the `.data` path (2026-07-06, GH #93 item 2)

Real `jq -s`/`--slurp` has one law: wrap the inputs in an array, always —
even a single document (`printf '{"a":1}' | jq -s length` is `1`, the array
length, not the object's key count). kaish's `jq` already got that right on
the text path (GH #80 landed real slurp framing there). But the structured
`.data` shortcut — the fast path used when an upstream stage like `fromjson`
or scatter/gather already handed over a parsed value instead of raw stdin
text — treated `-s` as a no-op, reasoning that "the pipeline is already
slurped." That reasoning doesn't hold: `.data` carries exactly *one*
document, the same as reading one document off stdin, and real jq wraps a
single document too. The divergence was silent and easy to miss because it
only shows up on scalar/record `.data` (an array `.data` piped through
`-s length` gives a plausible-looking wrong answer instead of an error).

The fix is a three-line change in `resolve_stdin_json`: wrap the `.data`
value in a one-element JSON array when `slurp` is set, same as the text path
already does. `docs/LANGUAGE.md`'s slurp section and the `JqArgs::slurp` doc
comment both asserted the old no-op behavior as intentional — both corrected
to describe the wrap. TDD: the existing `jq_slurp_is_a_noop_on_the_data_path`
test encoded the old (wrong) behavior as a passing assertion; it now fails
red against the fix and was rewritten in place as
`jq_slurp_wraps_the_data_path_value_in_an_array_of_one` plus two new
guard tests — one confirming an array-shaped `.data` value still gets one
more layer of wrapping (not passed through as "already an array"), and one
confirming plain `jq` (no `-s`) on the `.data` path is untouched. Full suite
and `clippy --all-targets` both clean.

## Hardening the job/tool-result seam (2026-07-06, GH #93 items 3, 4)

`kaish-types` had adopted `#[non_exhaustive]` broadly — `ExecResult`,
`OutputData`, `ToolSchema`, `ToolArgs`, `WriteMode`, `BackendError`,
`CommandKind`, `DirEntryKind` — but `job.rs` was missed. That gap had just bitten
an embedder for real: #96 added `JobStatus::Latched` and `JobInfo.latch` and had
to call it out as **BREAKING (embedders)** in the changelog, because a bare
`JobStatus`/`JobInfo` gave downstream `match`es and struct literals no forward
compatibility. Closing the gap now (`JobStatus`, `JobInfo`, `ToolResult`) means
the *next* variant/field addition is additive, not breaking — and softens that
existing #96 bullet from `**BREAKING (embedders):**` to a plain `Embedders:`
heads-up, matching the calmer framing Amy asked for (bug-fix-shaped changes
toward correct behavior, first-party/coordinated dependents, don't cry wolf).

`JobInfo` got a `new(id, command, status)` constructor plus
`.with_output_file()`/`.with_pid()`/`.with_latch()`, mirroring the existing
kaish-types builder idiom. `ToolResult` already had `success`/`failure`/
`with_data`; it picked up `.with_output()`/`.with_content_type()`/
`.with_baggage()`/`.with_latch()`/`.with_did_spill()`/`.with_original_code()` so
every field has a construction path across the crate boundary. `JobStatus`
needed no call-site changes at all — its variants are all unit variants, so
external construction was never blocked by `#[non_exhaustive]`; only its
(nonexistent, it turns out) external exhaustive matches would have needed a `_`
arm. Letting the compiler drive turned up exactly three struct-literal sites to
fix: two `JobInfo { .. }` in `scheduler/job.rs` and one test-only
`ToolResult { .. }` in `kernel.rs` — `cargo build --all-targets` was clean
immediately after, which is as much confirmation as this kind of attribute
change gets.

**Item 3** was a quieter bug found by reading `ExecResult` and `ToolResult`
side by side: `ExecResult.did_spill`/`.original_code` (the output-limiter's
"this got capped, and here's the code before the remap" signal) had no
`ToolResult` counterpart, so a backend-registered tool's (kaijutsu, an MCP
engine) capped result silently looked uncapped by the time it crossed back into
the kernel — in *both* `From` directions. Added both fields, propagated them
in both impls, and pinned it with round-trip tests in each direction.

**Item 4** turned out bigger than "align a test double." The test-only
`BackendDispatcher::dispatch` (used by `scheduler/pipeline.rs`'s unit tests, not
by any real embedder path) hand-rolled the `ToolResult → ExecResult` conversion
instead of calling `From<ToolResult> for ExecResult` — the exact function the
production dispatch path in `kernel.rs` uses. The hand-rolled version wrapped
`data` unconditionally as `Value::Json(json_data)`, which happens to coincide
with the real conversion for object/array-shaped data (both stay `Value::Json`)
but silently diverges for *scalar* `data` — the production path unwraps a plain
JSON number/bool/string into the matching native `Value` variant via
`json_to_value_no_envelope`; the old test-dispatcher path never did. It also
never touched `did_spill`/`original_code` at all, so item 3's new fields would
have been silently dropped on this path even after being added to `ToolResult`.
TDD caught both: wrote a scalar-unwrap test and a did_spill/original_code test,
confirmed each genuinely failed by temporarily reverting the dispatch.rs fix
and rerunning (red), then swapped the hand-rolled block for
`ExecResult::from(tool_result)` (green). A third test pins that envelope-shaped
`data` stays structured (`Value::Json`, never auto-decoded to `Value::Bytes`) —
it passes either way today, since neither path decoded envelopes, but it's
worth keeping now that both paths share one conversion function: any future
regression that adds envelope-decoding to the shared `From` impl trips it on
both the production and test-dispatcher side at once. No `CHANGELOG.md` entry
for item 4 — it's test-only internal churn with no embedder-visible effect.

## Interpreter allocation/stack pass (2026-07-05, GH #48)

#46/#47 landed a recursion depth guard sized against a measured ~380 KB of
native stack *per statement-engine re-entry level* — the figure that forced
`RECOMMENDED_STACK_SIZE` to 16 MiB. #48 is the pass to make that cheaper. A
model panel (gemini-pro + fable, whole hot files, no diff) had converged on a
ranked burndown, posted on the issue: profile fix first, then a batch of
mechanical boxing, then two wider items to measure-and-decide.

**The measurement had a trap.** The obvious tool, `-Zprint-type-sizes`, reports
the *coroutine layout* (future struct size), which is computed at MIR level and
is invariant to optimization. So it's a fine proxy for the boxing items (they
shrink the struct) but completely blind to the profile change and to
codegen-level native-stack cost — the thing #46/#47 actually cares about. Proof:
the debug and `opt-level=1` type-size logs were byte-identical. So the first real
deliverable was a *runtime* probe: a `stackprobe` builtin that steps into a
`#[inline(never)]` sync frame (the async body is heap-boxed by `async_trait`, so
a local there isn't the native stack) to read the true stack pointer at each
`$(…)` nesting level. Pure builtins never yield, so a nest runs in one
synchronous poll and adjacent probes differ by exactly one re-entry level. It
reads dead-consistent (min=max=median across 25 samples) — the recursion is
perfectly self-similar — and it became the metric every item was measured
against, plus a durable regression guard.

**The batch, each step measured (median per-`$()`-level, `opt-level=1`):**
- *Item 0* — `opt-level = 1` on the two interpreter crates: 414 → 106 KB debug
  (**3.9×**), nearly closing the debug↔release gap. fable's lead insight —
  unoptimized async poll frames are ~proportional to future size with redundant
  memcpys — landed exactly.
- *Item 1* (box the cold dispatch branches + the two `execute_pipeline` calls):
  106 → 96.
- *Item 3* (drop the per-command tracing spans off the ring): 96 → 76 — the
  `Instrumented<Span>` wrapper was heavy on the big ring futures.
- *Item 4* (box the command-subst scope snapshot): 78.7 → 77.8 KB.
- *Item 2* (box the two per-command `ExecContext` snapshots via a sync helper,
  collapsing two near-duplicate 30-field blocks into one): 77.8 → 74.6.
- *Item 7* (drop the `ToolSchema` before the execute await): no-op under
  optimization (the compiler already narrows it), kept for debug + clarity — and
  reported honestly as such.
- *Item 8* (`tool_schemas: Vec → Arc<[…]>`): an *allocation* win (the ~70-schema
  catalog was deep-cloned per dispatch → refcount bump), invisible to the stack
  probe, so validated by construction.
- *Item 5* (box `ExecResult.output` and `Scope.last_result`) — **the headline:**
  74.6 → 56.8. `ExecResult` is the most-replicated type in the recursion frame
  (every `ControlFlow`, every return, the accumulator), so boxing its 120-byte
  `output` cascaded ~10× per level for a single 14 KB/level drop.

**Result: ~46% off the per-level native stack** — release 92 → 50 KB, debug
opt=1 106 → 57 KB, debug no-opt 414 → 193 KB.

**Two decisions were Amy's.** The wider items 6 (box `Value::Json`) and 9
(Arc-split `ExecContext`) turned out poor value once re-measured: `Value` is only
72 B and no longer dominant, and item 9's benefit largely evaporated — item 2
already boxed `ExecContext` off the stack and item 8 already Arc'd its expensive
clone, leaving a large invasive refactor for a small allocation gain. Deferred
both (measure-first follow-ups on #48). And with the frames this much smaller,
the #46/#47 pair was relaxed: `MAX_RECURSION_DEPTH` 32 → 48 and
`RECOMMENDED_STACK_SIZE` 16 → 12 MiB, adjusted *together* with a comment making
the relationship explicit — the cap must trip before `cap × (worst-case
per-level) < floor`, and the worst case is deliberately the ~193 KB *unoptimized*
figure because the new `opt-level=1` dev profile is local to this workspace and
does **not** propagate to embedders, whose own debug builds pay the full cost.
`48 × 193 KB ≈ 9.3 MB` under 12 MiB keeps the same ~1.3× margin the old pair had.

kaibo (deepseek, holistic, pointed at the worktree) reviewed all six change
classes clean — no drop-order/lock-lifetime issue from the `Box::pin`s, faithful
field parity in the snapshot helper, correct accessor boxing, identical serde
wire format, sound `Arc` sharing, and adequate margin math. One flagged
"output_limit sync" was pre-existing code it noticed while reading, not part of
the change.

## Surfacing the backgrounded confirmation latch (2026-07-05, GH #96)

The confirmation latch (`set -o latch`) went first-class in #92, but a gap
survived: background a gated op — `rm precious.txt &` — and the latch is stored
in the job's result, but *every* consumer was blind to it. `wait` reported
`Failed`; `jobs`, `JobInfo`, and `/v/jobs/{id}` had no latch anywhere;
`Job::status()` folded the exit-2-latch into `Failed`, indistinguishable from a
real error. The op stayed safely *blocked* (no data loss), but the nonce was
unreachable, so a backgrounded gate could never be *fulfilled*. Low-stakes, but
a real dead-end in the safety story.

The fix threads the stored `LatchRequest` out to all four surfaces named in the
issue, each with one job: `JobStatus::Latched` (the state is *held*, not
errored, so `jobs` and `/v/jobs/{id}/status` say so distinctly);
`JobInfo.latch: Option<LatchRequest>` (the programmatic surface an embedder
reads from `JobManager::list`/`get`); `wait` surfacing the request on its
result's control-plane `.latch` field with exit 2, mirroring a foreground gate
so `latch_request()`/`Kernel::confirm` work identically; and a new
`/v/jobs/{id}/latch` VFS node rendering the request as JSON (nonce, command,
paths, hint) — empty body when the job isn't gated, so a reader reads-then-parses.

TDD, and the capstone test earns its keep: background a gated `rm`, `wait 1` to
surface the latch, `Kernel::confirm(&latch)`, assert the file is gone — the
whole loop, from backgrounded gate to fulfillment, in one test. Two design
touches worth noting: `wait` on several gated jobs keeps the *first* latch
(`.latch` holds one; waiting on multiple gated jobs is an odd pattern), and the
VFS node stays plain-text-empty rather than error when not latched, matching how
`status`/`command` always read. Three kernel-routed tests red→green, 4377 suite
+ clippy `--all-targets` clean, verified end-to-end through the REPL.

## The recursion guard, and why #46 and #47 are one PR (2026-07-05, GH #46/#47)

The plan was a tidy correctness fix: thread a depth counter through the three
dynamic re-entry points (command substitution, shell functions, `.kai`
scripts), return a loud error past a cap, done. The counter + RAII guard part
*was* tidy (an `AtomicUsize` on the Kernel, fresh per fork, decremented on drop
so cancellation stays balanced). Picking the **cap** is where physics showed up.

We measured the native stack cost per recursion level by disabling the cap and
probing depth-at-overflow: **~380 KB/level in debug, ~80 KB/level in release**
(the fat boxed async futures #48 is about). That means:

| thread | debug | release |
|---|---|---|
| 8 MB main | ~21 levels | ~100 |
| 2 MB tokio worker | ~5 | ~25 |

The plan had been "defer #47 (stack size) to a doc note, ship #46 alone." The
measurement killed it: with the default stacks, **no single cap works** — one
safe on a 2 MB worker (~4) is uselessly shallow, and a useful cap (~16) doesn't
protect workers *or the test itself* (a `#[tokio::test]` thread is ~2 MB, so a
recursion test would SIGSEGV at ~5 before it could assert the loud error). #46
needs #47's controlled stack to be effective *and* testable. Amy called it:
ship them together.

So the guard is tuned for a **documented floor**: `RECOMMENDED_STACK_SIZE`
(16 MiB) and `MAX_RECURSION_DEPTH` (32), both `pub` so embedders can size their
runtime against them. kaish can't set the stack itself (it doesn't own the
runtime), so the REPL walks the talk — `thread_stack_size` on its tokio workers,
and a `std::thread` with the recommended stack driving `block_on` (the OS main
thread's ~8 MB overflows a deep debug recursion before the cap). The tests run
each recursion on a `RECOMMENDED_STACK_SIZE` thread with a fresh current-thread
runtime — the only honest way to reach the cap without the overflow-under-test
taking down the binary. Verified end-to-end: `f(){f;};f`, mutual recursion, and
`$()`-nested recursion all go loud (debug and release), foreground and inside a
pipeline stage; `countdown 20` still runs.

One consistency note banked: a recursion error *inside* `$(...)` follows kaish's
existing command-substitution semantics — a failed `$()` yields an empty
expansion and doesn't fail the enclosing command (same as `echo $(false)`), so
that path is *bounded* (no crash) but not exit-code-loud. Direct recursion
(functions, scripts) is exit-1-loud. Not a new silent path — it matches how
`$()` already behaves; making `$()` failures louder is a separate concern.

Meanwhile fired gemini-pro + fable batches at the kernel/types source to hunt
per-level stack reductions (#48 territory) — shrink the ~380 KB and the floor
can drop. That's a follow-up; this PR ships the guard + the floor it needs.

## `$()` in a redirect target — the bug that only bit embedders (2026-07-05, GH #90)

Picked this up expecting a quick "attach the dispatcher for bare commands" fix.
The first surprise: the exact repro (`echo x > $(echo f.txt)`) **worked** through
the REPL. The `Stmt::Command` fast path was removed months ago ("This is the
single execution path — no fast path for single commands"), so bare commands run
through `execute_pipeline`, which attaches `ctx.dispatcher`. The GH #90 comment
in `redirect_in_cmdsubst_tests.rs` describing it as blocked was stale — or so it
looked. I nearly closed it as already-fixed with a pinning test.

The pinning test is what saved it. Written through `kernel.execute()` (the
embedder API, not the REPL), all three cases went **red**: `eval_redirect_target`
fell back to the sync evaluator and failed with "could not evaluate redirect
target". The REPL worked; `kernel.execute()` didn't. The tell: `ctx.dispatcher`
is only populated by `self.dispatcher()`, which upgrades a `self_weak` that's set
**only in `into_arc`**. The REPL Arc-attaches its kernel; a bare `Kernel` from
`Kernel::new()` — every embedder holding a `Kernel` by value, and the entire
4000-test harness — never sets `self_weak`, so `dispatcher()` returns `None` and
a `$()` redirect target silently degrades. #90 was real, and it bit *exactly the
surface that matters* (kaibo/kaijutsu drive `kernel.execute`), while hiding from
the interactive shell we test by hand.

The fix follows the constraint: without an `Arc<Kernel>` there is no
`Arc<dyn CommandDispatcher>` to put in `ctx.dispatcher`, so the owned-handle
approach can't work for a bare kernel. But the *runner* always holds a real
`&dyn CommandDispatcher` (the kernel itself, Arc'd or not). So thread it: through
`apply_redirects` and `setup_stdin_redirects` into `eval_redirect_target`, which
now calls `dispatcher.eval_expr` directly and no longer reads `ctx.dispatcher` or
falls back to the sync evaluator. The silent fallback — the thing that turned a
missing dispatcher into a wrong answer instead of a loud error — is deleted
outright. Bonus coverage: a pipeline-stage target (`echo x | cat > $(echo g)`)
had the same gap (the stage context copies `dispatcher: None` too) and is now
fixed and pinned. Four kernel-routed tests, red→green; 4374 suite + clippy clean.

Lesson banked: **test the embedder path, not the REPL.** A convenience wrapper
that Arc-attaches can paper over a defect on the API every real consumer uses.

## `grep -r` searches a file operand instead of silently missing (2026-07-05, GH #105)

The reflex `grep -r PATTERN FILE` — `-r` is muscle memory, and everyone assumes
handing a file to a recursive search searches *that file* — returned zero matches,
exit 1, and **no error**. The recursive branch unconditionally treated the operand
as a walk root and enumerated it with a files-only `**/*`; a directory yields its
files, a plain file has nothing "under" it, so the walk collected nothing. The
worst failure mode: silent. A model reads the empty result as "not found" and
quietly corrupts whatever reasoning follows — bad enough that kaibo carries a note
in its shell preamble steering models around it, spending instruction tokens every
session.

The fix reframes what `-r` *means*: it governs how **directories** expand, nothing
more. The recursive branch now partitions its operands by `stat` — files are
searched directly (falling through to the ordinary file-operand path, so a lone
file prints unprefixed like plain `grep -c`), directories are walked, and a mixed
`grep -r p file dir` does both. A stat-unresolvable operand still falls to the
walker, preserving pre-fix behavior for a bad root. Display is conservative: the
sole-directory walk strips its root exactly as before (byte-for-byte unchanged
output for the overwhelmingly common `grep -r p dir`); only the genuinely-new
mixed/multi-source shapes switch to cwd-relative prefixes so each source shows
under its own subpath. TDD: four kernel-routed regression tests (single file, `-c`
count, `-R` alias, mixed file+dir) red first, then green; 4370-test suite and
clippy `--all-targets` clean. Once this ships, kaibo's preamble note can go.

## The `test` builtin — `[[` semantics as a command (2026-07-04)

`test` had been quietly shelling out to the host `/usr/bin/test`: OS-dependent,
not VFS-aware, and command-not-found in a no-subprocess build. Amy asked whether
we could add a `test` that follows kaish's own `[[` semantics and whether that
would be safe for muscle memory. A 4-model kaibo panel (deepseek/gemini/or-gpt/
or-kimi, stateless) converged hard: yes — but a flag-form-only shim is
*not credible*, because `test a = b` is the canonical idiom, and without it the
thing would parse-error on the one form everyone reaches for. Full scope it was.

**Two parts, and the second was the surprise.** Part one was grammar relief: kaish
lexes `=`/`==`/`!=`/`!` as shell-significant tokens, so `test a = b` /
`test ! -f x` parse-errored before reaching a command. The fix is a small,
name-agnostic arg-parser production making those four operators literal positional
words (angle brackets `< > <= >=` deliberately excluded — they stay redirection).
A side effect, bash-consistent: spaced `cmd key = value` now parses as a three-arg
command instead of an error (glued `key=value` stays an assignment).

Part two — the builtin — turned out to be *not* about the predicate logic (that
reuses `[[`'s engine verbatim), but about **argv shape**. POSIX `test` is
position-sensitive (`test $x = -n`, `test 0 -gt -5`), while kaish's `ToolArgs`
splits flags into an unordered set — so an operand that merely *looks* like a flag
gets silently mis-routed (proof: `echo a -n b` prints `a b`). Amy pushed on
whether the lexer could just preserve order always; the honest trace showed order
dies in the *binder*, not the lexer, and that faithful source order is a distinct
binding mode incompatible with the flag-value-consumption logic (`grep -A 3`
consumes its `3`). So: a first-class opt-in `ToolSchema.raw_argv` — the binder
binds every arg to `positional` in source order with `Value` types preserved.
`test` opts in; nothing else pays. (A first-class field, not a reconstruct-and-
shape-check — the same principle as the latch below.)

**One decision reversed on the merits.** The panel wanted integer-strict numerics
(POSIX errors on `1.5 -eq 1`). Amy pointed out that kaish numbers *are* JSON
numbers and `[[` already compares floats via this exact `numeric_compare` —
rejecting `1.5` in `test` but not `[[` would be the odd one out. She was right,
and it *simplified* the code: `test` numeric == `[[` numeric, verbatim, no
integer-strict wrapper. Still loud on non-numeric/collection/NaN.

Predictable-by-design divergences from POSIX `test`, all deliberate: no word
splitting; `-a`/`-o`/`( )` rejected loudly (chain with shell `&&`/`||` or use
`[[ ]]`); no arg-count magic (an operator missing its operand is loud, not a
surprise-true); negation is a single parity-collapsing leading `!`. Adding the
builtin retired the `PosixTestCommand` validator advisory (W006) that used to
steer `test` toward `[[` — it's a real command now.

**Sequencing.** Built on a worktree parked while #94 (hardening) and #92/#97
(latch) landed — the `raw_argv` work touches the same dispatch seam #92 reworks,
so it went *after*, beside `current_invocation`, not in conflict. Then rebased
across #99 (chumsky alpha.8 → 0.13); the parser change compiled clean on the new
combinator surface.

## The confirmation latch becomes a first-class, fulfillable API (2026-07-04)

Fell out of the redirect-in-`$()` work above. That change had to special-case
`clear_stdout` so a stdout redirect wouldn't clear the `rm` latch nonce — because
the nonce rode inside `ExecResult.data` as serialized JSON, overloading the
data-plane `.data` (structured stdout) with a control-plane signal. Amy's call:
don't slime the latch through another feature's field with a runtime shape-check
— give it a first-class typed home.

**The field.** `ExecResult.latch: Option<Box<LatchRequest>>`. `latch_result`
sets it typed (no serialize round-trip); `latch_request()` reads it; `clear_stdout`
drops the discriminator and clears `.data` unconditionally. Threaded through the
`ExecResult`↔`ToolResult` backend roundtrip (which #94 had just converted to
symmetric `From` impls, fixing the old #84 field-drop) and through
`accumulate_result` — a single `rm x` statement flows through accumulation, so
forgetting `.latch` there lost every gate (a test caught it). `--json` surfaces
it under a dedicated `latch` key, never folded into `data`.

**Then Amy pushed on fulfillment.** Inspection was first-class (`latch_request()`),
but *fulfilling* a latch meant re-running with `--confirm=<nonce>` — via the
`hint` string, which is `format!`-built and doesn't quote paths (a space breaks
it), or by manually rebuilding argv. She wanted the latch to hold the exact state
for a precise replay. So: capture the exact argv at the dispatch seam
(`(dispatch_name, ToolArgs::to_argv())`) into `LatchRequest.tool`/`.argv`, and add
`Kernel::confirm(&LatchRequest)` that replays `execute_argv(tool, argv)` with
`--confirm` **prepended** (appending would let `to_argv`'s trailing `--`
terminator swallow the flag). A path with a space now round-trips exactly where
the hint can't — the payoff, pinned by a test. The seam capture is gated on
`latch_enabled` so it's free when the latch is off.

**Two stack-size lessons, the hard way.** `ExecContext` and `ExecResult` are both
rebuilt/returned at every level of deep `$()` recursion. Adding an inline
`(String, Vec<String>)` to `ExecContext`, and growing the inline `LatchRequest`
inside `ExecResult` by two fields, fattened every frame enough to overflow the
stack on `deeply_nested_command_substitution` — but only under `cargo test --all`
(parallel binaries, tighter thread stacks; it passed in isolation). Boxing both
(`Box<(String, Vec<String>)>`, `Option<Box<LatchRequest>>`) kept the hot structs
lean and fixed it. Lesson: a ~150-byte field on a stack-hot, deeply-recursive
struct wants a box (see GH #46/#47). Docs (EMBEDDING.md, LANGUAGE.md, README,
the syntax fragment) reworked to teach the typed field + `confirm`.

## Redirects inside `$()`, and what that taught us about `.data` (2026-07-04)

Started from a curiosity: a test comment said "kaish's grammar doesn't accept a
redirect inside `$(...)`." It turned out to be a one-line gap — `cmd_subst_parser`
hardcoded `redirects: vec![]` and never called `redirect_parser` — but wiring it
up surfaced two deeper things.

**The parser cycle.** Naively calling `redirect_parser()` from the cmd-subst body
stack-overflowed at *parse* time: `redirect_parser` built `primary_expr_parser()`
for its target, `primary_expr_parser` builds a `cmd_subst_parser`, which now calls
`redirect_parser` again — an unbounded *construction* cycle. Fix: parameterize
`redirect_parser` on its target parser. The top level passes a fresh
`primary_expr_parser()`; the cmd-subst body passes its already-recursive `expr`
handle, so the nested target flows through chumsky's existing `recursive` wrapper
instead of constructing a new parser at each depth. `$(cmd > $(subst))` now parses.

**What a stdout redirect means for `.data`.** The interesting part. `$()` capture
prefers a result's structured `.data` over its text, so `x=$(seq 1 3 > file)` was
capturing `[1,2,3]` instead of bash's `""` — the redirect sent the text to the
file but left `.data` intact. We decided `.data` is *the structured view of
stdout* (every builtin that sets it — seq/jq/fromjson/keys/find — does so as the
typed form of what it wrote to stdout), so a stdout redirect must take `.data`
with it. New `ExecResult::clear_stdout()` clears `.out`/`.output`/`.data` together;
the three file-redirect arms and (caught in review) the `1>&2` merge arm all route
through it. This also kills a pipe-sideband leak (`cmd > file | consumer`) and
makes `for x in $(cmd > file)` correctly iterate zero times.

**Two things this disturbed.** First, four `kaish-last` tests broke — they used
`producer > /dev/null; kaish-last` where the `> /dev/null` was a *harness trick*
to silence the producer's stdout in the shared capture stream (the line-22 comment
said so), and leaned on `.data` surviving that redirect. That reliance was an
accident of the original `.data` rollout, not a contract; rewrote them to isolate
kaish-last's output by its last line, no redirect. Second, and more important:
`.data` is *overloaded* — for `rm` under `set -o latch` it carries the
confirmation nonce, a control-plane signal, not stdout. Blanket-clearing `.data`
silently disabled the safety gate on `rm precious > log` (a TDD guard caught it).
The scoped fix: `clear_stdout` preserves `.data` when `latch_request()` matches
(exit-2 + the exact `LatchRequest` envelope). Amy's call: that overloading is a
real smell, but the latch deserves a first-class typed public-API field of its
own rather than being refactored as a rider on this change — deferred to its own
worktree so neither feature's boundary gets weakened.

Cross-family review (DeepSeek + Gemini via kaibo, whole files, no diff) converged
independently on the one bug this text glosses: the `1>&2` arm cleared only
`.out`, leaking `.data`. Fixed, with a guard test. Review also surfaced GH #90 —
`$()` in a redirect *target* fails for a bare single command (dispatcher not
attached) — a pre-existing bug, orthogonal, filed not fixed here.

## scatter/gather's own flag values could silently drop a bad subscript (2026-07-03)

A 0.11.0 pre-release punch-list item: `scatter`/`gather` bind their OWN flag
values (`--as`, `--limit`, `--timeout`) through a *sync* twin of the real
argument binder — `build_tool_args`/`eval_simple_expr` in
`scheduler/pipeline.rs` — because `run_scatter_gather` parses those flags once,
before any worker forks, and can't recurse back through the async
`PipelineRunner::run` → `Kernel::dispatch` chain to get there. Every real
command's arguments bind through the async `build_args_async` (`kernel.rs`),
which already surfaces a `PathError` (missing key, shape mismatch, undefined
subscripted root) loudly — matching the other three primary eval sites
(assignment, `$(( ))`, `"${…}"`). The sync twin never got that treatment: every
arm discarded the error via `.ok()` / `if let Ok(..)`, so `scatter --as
${u[nope]}` silently fell back to treating `--as` as a bare boolean flag
(dropping the intended value and stranding the bad expression as an unused
positional) instead of failing, and `${#u[tags]}` on a bad subscript inside an
interpolated flag value silently omitted the length instead of erroring.

Confirmed `build_tool_args` has exactly one production call site —
`run_scatter_gather`'s two calls parsing the `scatter`/`gather` commands'
own args — everywhere else it's `#[cfg(test)]` (`BackendDispatcher`) or the
~19 unit tests inside `pipeline.rs` itself. Gave `eval_simple_expr` and
`eval_string_parts_sync` a real `Result` error channel: `Ok(Some(value))` on
success, `Ok(None)` unchanged for "not representable in this reduced sync
context" (binary ops, command substitution — still the documented, deferred
"eliminate the sync twin" gap), and `Err(msg)` for a genuine `PathError`
(absence/shape), propagated with the same message text the async path uses.
Also noticed `eval_simple_expr`'s top-level match had no arm at all for a
*bare* (unquoted, whole-token) `Expr::VarLength`/`Expr::VarWithDefault` —
`scatter --limit ${#tags}` fell to the catch-all `_ => None` — so added those
two arms routing through the same `resolve_length`/`resolve_default` the async
path and the in-string arms already call. `build_tool_args` is now fallible;
`run_scatter_gather` converts an `Err` into `ExecResult::failure(1, …)` at the
same architectural boundary `run_single` already uses for a dispatch error, so
the failure surfaces as a normal loud pipeline error, not a panic. Along the
way, deduped `eval_simple_expr`'s `Expr::Interpolated` arm (it had its own
~60-line copy of `eval_string_parts_sync`'s loop) down to a single call.

TDD: wrote the new tests first against the unmodified source (verified two of
them failed as expected — a bad subscript and a shape error in `--as` both
silently exit 0), then applied the fix and confirmed all pass. One test needed
a second pass — its worker read `$N` while the buggy fallback actually bound
`$n` (a mangled but working var name), so it was accidentally passing "for
free" on the old code for an unrelated reason (the worker's *own* command args
already error loud via the async path); fixed to read `$n`, matching the
silent-fallback name, so it's a real red/green pair now. Left the parallel
`StringPart::Arithmetic` swallow in the same two functions alone — same
symptom class, but a distinct `anyhow` error type (not `PathError`), so out of
this fix's scope; recorded in issues.md for the next pass.

kaibo's post-PR review caught a hole in the first cut: `eval_simple_expr`'s
`Expr::VarRef` arm coalesced `PathError::UndefinedRoot` to `Ok(None)`
*unconditionally*, so `scatter --as ${x[key]}` with a typo'd (entirely
undefined) root still silently dropped the flag — UndefinedRoot isn't
`Absence`. Follow-up commit restricts the coalesce to bare paths
(`path.segments.len() <= 1`), erroring on subscripted paths with the same
`"${x[key]}: undefined variable"` shape `resolve_length` uses (exported
`format_path` as `pub(crate)` for it). Deliberately did NOT apply the same
restriction to `eval_string_parts_sync`'s `StringPart::Var`: verified
empirically that both primary string-interpolation sites (async
`eval_string_part_async`, sync `eval_interpolated`) expand an undefined root
to empty even when subscripted (`echo "a${nope[k]}b"` → `ab`, bash-compatible)
— restricting only the sync twin would have made it *stricter* than the
primaries, diverging the other way. The whole-token/string-context split is
the shipped contract; the sync path now matches it on both sides. Also
confirmed the bare `Expr::VarLength`/`VarWithDefault` arms added in the first
cut are genuinely reachable (parser emits both variants in expression
position, `parser.rs:2307`/`:48`) and pinned them with observable tests —
the `VarWithDefault` one exploits the fact that a dropped `--as` leaves the
ITEM binding in place, so the workers' `$W` fails loud if the arm regresses.

---

## `/dev` was a no-op under `with_backend` (2026-07-03)

Amy asked a throwaway question — "did we ever add `/dev/null`?" — which turned
into finding kaijutsu's kernel never had it. kaijutsu builds its kaish kernel
via `Kernel::with_backend` (custom-storage embedders), which never ran the
`setup_vfs()` path that mounts `DevFs` at `/dev` for `Kernel::new`/`transient`.
Checking whether kaijutsu's own read-only host-root mount shadowed `/dev/null`
surfaced a second, deeper bug: it does, for reads (the real host `/dev/null` is
empty too), but writes go through `LocalBackend::read_only`'s guard and error
as read-only instead of discarding — so `cmd > /dev/null` was actively broken,
not just missing.

Worse: mounting `DevFs` at `/dev` inside `with_backend` alone would have been a
no-op. `VirtualOverlayBackend::is_virtual_path` hardcoded routing to `/v`/`/v/*`
only; every other path, including a freshly-mounted `/dev`, fell straight
through to the embedder's own backend regardless of what the internal
`VfsRouter` had mounted. Fixed both: `VfsRouter::has_mount` exposes a
mount-table lookup, `is_virtual_path` became an `&self` method that also
checks it (keeping the `/v` reservation as an explicit fast path, since that
whole namespace is reserved even where nothing is mounted), and `with_backend`
now mounts `/dev` alongside `/v/jobs`/`/v/blobs`. Verified the regression test
actually catches the bug by reverting the two source files and rerunning it —
all three cases failed as expected before the fix, passed after.

kaijutsu itself stays broken until it bumps its `kaish-kernel = "0.10"`
crates.io pin to whatever ships this — tracked as a follow-up, not fixed in
this PR.

## `help regex` — waking the ERE weights (2026-07-03)

PR #65's last follow-up: the BRE-superset story lived in four places (grep
schema, sed schema, awk help, LANGUAGE.md) with no single teachable surface.
Amy's framing set the design: token-efficient, and written to *wake up the
model's ERE weights* — so the fragment leads with working ERE idioms
(alternation, capture groups, quantifiers) before mentioning BRE compat at all,
then covers the two escape hatches and the one hard limit. One screen, every
line verified against the binary. Rides the #69 `syntax_section` mechanism, so
`help regex` worked the moment the fragment was named.

## The two review bugs: silent-zero length + literal-`$k` record keys (2026-07-03)

The 2026-07-03 coverage review verified two live silent-wrongs; both fixed here.
`${#nope[items]}` returned 0 — `resolve_length`'s bash-parity arm (`${#unset}` →
0) didn't distinguish bare roots from subscripted paths, so a typo'd name in a
length-guarded loop spun zero times with no diagnostic. Subscripted paths now
error like bare `${nope[items]}` does; bare `${#unset}` stays 0, pinned
separately so the forms can't drift together.

`{"$k": 8080}` created a literal `"$k"` key — the record-literal parser took
`Token::String` raw. Double-quoted keys now interpolate like every other
double-quoted string (new `RecordKey::Interpolated(Vec<StringPart>)` riding the
existing StringPart machinery in both eval sites; a pure-literal parse folds
back to `Quoted` so the common case is free). Single quotes remain the
literal-`$` escape hatch, and an unset var in a key expands to `""` — the
ratified string-interpolation rule applied consistently, pinned with a test.

---


## Collections panel gate + docs delivery — sign-off (2026-07-03)

The last item on the collections milestone: Teaching note #8's pre-sign-off cross-model
panel re-test against the *final* bracket surface (literals, lvalues, `push`, `$()`-only
iteration — all merged to main this session via #66/#67), plus closing the docs/help gaps
that closing out that milestone exposed.

**The composable-help surface had drifted from `LANGUAGE.md`.** Each collections PR kept
`docs/LANGUAGE.md` in sync (per the CLAUDE.md convention), but the `collections`
`syntax_section` in `crates/kaish-help/src/fragments.rs` — the single source for `help
syntax`, `syntax.md`, and (new) `help collections` — was never updated when native literal
construction and spread landed (#66/#64). It still taught `fromjson`-only construction.
Fixed: the syntax_section now leads with the native literal forms (list/record/nesting) and
the `...` spread nest-vs-flatten contrast, matching LANGUAGE.md; a new ranked Foundations
Rule (`collection-literals`, rank 10) and Contrast fragment mention the literal forms and
the dot-leakage error in the always-on onboarding block too.

**`help collections` is now a fragment query, not a file** (the design's explicit decision,
finally implemented) — `compose::render_syntax_section(key)` renders a single `Syntax`
fragment by its key, single-sourced with `syntax.md`, and `HelpTopic::parse_topic` falls
back to it for any key that matches a registered syntax_section before falling through to
`Tool(name)`. The mechanism is generic (any future subsystem-sized syntax feature gets
`help <key>` for free by naming its section), not collections-specific plumbing.

**A real regression surfaced while assembling the panel's cheat sheet, independent of the
panel run itself.** Teaching note #1 — "teach an operator inside its full control
structure, never bare" — is the hard-won rule from the *original* 2026-06-05 experiments
(a standalone `[[ k in $r ]]` line reads as a complete statement to a model). Both the
`collections` and `test-expressions` syntax_sections had shipped membership as a bare
standalone `[[ ]]` line anyway — introduced when membership landed (#58) and never caught,
because no panel re-test had exercised the *actual composed artifact* since then. Fixed in
both fragments and in LANGUAGE.md's matching examples before running the panel, so the
tested artifact reflects the corrected teaching copy rather than the regression it would
otherwise have quietly re-validated.

**The panel gate: 18/18 clean, zero correction rounds.** Ran the actual shipped
`Recipe::agent_onboarding()` output plus the `collections` syntax_section — the real
delivery artifact, not an ad-hoc cheat sheet — as a stateless one-shot prompt (no repo
access) against DeepSeek V4, Gemini 3.5-flash, and Claude Haiku 4.5, on a 6-task script
covering every item Teaching note #8 called out: nested record construction, bracket-path
lvalues + `push`, the literal-vs-variable subscript distinction (`${user[name]}` vs
`${user[$field]}`), membership inside a full `if/then/else`, a bare dot-leakage probe, and a
slice. All three models converged immediately on `for k in $(keys $servers); do echo "$k:
${servers[$k][port]}"; done` — the exact form the 2026-06-05 panel most commonly got wrong
(it required explicit correction to stop reaching for the bare-builtin for-head). Zero
dot-leakage on the bare field-access task, despite no "don't use dots" warning in the
prompt — the taught contrast (`${u.name}` shown as the wrong form, with its error) held
unprompted. All 18 generated scripts were then run against the real
`./target/debug/kaish` binary and produced correct output. **The v2
bare-collection-iterates relaxation is not adopted** — there's no evidence the `$()`-only
form is a tax being paid forever; it converged in one round across the whole tested range.
The arrays-and-hashes design doc carried the full result inline at Teaching note #8 and the
Resolved-decisions "Access form" entry, rather than a separate write-up, since that doc was
already the design's evidence record. (It has since been retired; the teaching notes live in
`docs/designing-syntax-with-llms.md`.)


## BRE follow-ups + the stale-`$?` bug (2026-07-03)

Working the PR #65 follow-up comments: awk's invalid-FS/`split()` errors now name
the separator as the user wrote it (not the rewritten form the engine saw) and
carry the dialect hint when the rewrite changed it.

The bigger catch was the loose end from PR-D testing: a standalone `[[ ]]` never
wrote `$?` — `[[ 1 = 2 ]]; echo $?` printed 0, so `[[ -f x ]]; ok=$?` silently
read the *previous* command's status. Amy called it P1 on sight and it was
cheaper to fix than file: `Stmt::Test` now mirrors `Stmt::Command` (write the
result, honor suppressible errexit). The chain arms already suppress errexit
around their left side and `if`/`while` conditions evaluate as expressions, so
`[[ … ]] && cmd` and loop conditions are unaffected — pinned with five
`shell_compat!` tests verified against real bash, including the `set -e` trip.

## GNU BRE superset for grep/sed/awk (2026-07-03)

Issue #60 measured `grep 'a\|b'` as the single largest source of wasted agent
tool calls (12 of 41 explorer greps in one sweep, each followed by a single-term
retry) — commercial models write GNU BRE reflexively and don't unlearn it from
preamble text. kaish had three different wrong answers: grep silently no-matched
(literal `|`), sed loud-rejected (the E006 teaching error from the ergonomics
pass), awk silently no-matched. The fix reverses the sed-pass decision that
erroring beats guessing: a shared rewriter (`regex_dialect.rs::bre_metas_to_ere`)
turns the seven backslash-metas (`\| \+ \? \( \) \{ \}`) into their ERE forms, so
both spellings are the same operation and it's no longer a guess. `-E`/`-r`
(grep/sed) now mean *strict ERE* — the escape hatch where `\|` is a literal pipe;
awk always rewrites (no flag). The narrow casualty: a backslashed meta is always
the operator; literals move to bracket classes (`[|]`, `[)]`).

The review round (self-review + kaibo deepseek on whole files, no diff) earned
its keep: awk's FS/`split()` path applied the rewrite *inside* the multi-char
regex branch, so the gawk literal-pipe idiom `FS="\\|"` became the
empty-alternation regex `|` and split every character (NF=7 on `a|b|c` vs gawk's
3) — silent-wrong, the exact class the branch exists to kill. gawk's actual
order is demote-then-single-char-literal, so the fix is rewrite *before* the
single-char check. Also caught: the `-E` flag doc comments still said "no-op"
(and they're schema-visible via `schema_from_clap`), and the changelog claimed
grep takes `-r` (that's recursive). Since a formerly-literal escape like `:\)`
now fails compile with an error describing a pattern the author never wrote
(`unopened group` on `:)`), compile errors append a dialect hint (bracket-class
spelling + the strict-ERE flag where one exists) whenever the rewrite actually
changed the pattern.

## Collection lvalue writes + `push` land (2026-07-02)

`xs[0]=9`, `user[email]=x`, deep paths (`services[web][port]=9000`), and a
bareword `push` — the write half of the collections effort, on top of the
shared per-hop path resolver (`resolve_step`, from the read-side #6 work) and
the collection-literals grammar.

**`walk_write` mirrors the read descent, sharing `resolve_step` unchanged, but
diverges at the leaf.** The read side (`resolve_path`) walks the tree with
`Cow`-borrowed lookups; the write side clones the root once, then walks
`&mut serde_json::Value` chains so mutating the deepest hop mutates the whole
tree in place (no per-hop clone). Every hop still calls the SAME `resolve_step`
for classification (bounds, shape) — read and write can never classify a
subscript differently. The two walks diverge only in *leaf policy*: an
intermediate hop requires the child to already exist (`descend_mut` — no
autovivification, mirroring the read's `descend`), while the FINAL hop may
insert a new record key (`apply_leaf_write`) — the one thing a path-set may
create. A list index write is in-bounds only for free: `resolve_step`'s
`classify_index` already turns an out-of-bounds index into a loud `Absence`
before the write ever sees it. A slice step (`xs[0:2]=x`) is always a `Shape`
error, at any hop — mutating through a detached slice copy would silently not
write back to the real list, so it's rejected outright rather than special-cased
only at the final position.

**The lexer needed a SECOND, distinct suppression trigger.** The
collection-literals grammar already taught the lexer to stop fusing a
`[`-leading run into a `GlobWord` at *value position* (RHS of `=`/`in`). An
assignment LHS (`fruits[0]=kiwi`) sits at argv position, not value position —
a different shape entirely. `flush_glob_run` gained `followed_by_eq`: computed
by peeking at whatever token triggers the run's flush (in token-stream order,
regardless of whitespace, since `local xs[0] = 9` and `fruits[0]=kiwi` must
both suppress), true only when that token is `Token::Eq`. A bracket-pair run
with no `*`/`?` immediately before `=` is a subscripted assignment target, not
a glob, and gets suppressed the same way the value-position case does — the
two triggers are ORed together in `flush_glob_run`, each protecting a
different shape of run.

**`Assignment.name: String` widened to `Assignment.path: VarPath`** — no dual
representation, every construction/read site updated in the same change
(parser, kernel, sexpr formatter, validator, tests). `Assignment::name()`
extracts the root `Field` for the still-common bare case; a subscripted write
never honors `local` (it always mutates the existing root wherever it lives —
`local` is meaningless once you're inside an existing collection).

**Env-prefix assignment stays bare-ident-only, by construction, not by a
runtime check.** `env_prefix_assign` still calls `ident_parser()` directly
(never `lvalue_path_parser()`), so a subscripted target can't even reach the
`EnvScoped` grammar arm — structured values can't cross the process boundary
anyway. Grounding this surfaced a pre-existing, unrelated quirk: kaish's
statement `terminator` is `.repeated()`, not `.at_least(1)`, so `X=1 Y=2` (no
separator at all) already parsed as two independent assignment statements
before this change. `user[email]=x echo hi` falls through the same way —
NOT into `EnvScoped` (verified with a parser test), but into two ordinary
statements. That's the existing grammar's behavior extended consistently, not
a new hazard this work introduced.

**Validator additions (E016, E017).** A subscripted assignment whose root
isn't bound (`z[0]=x`) is now a static error with a "create it first" hint,
the same treatment `push`'s undefined-target rule gets at runtime. A dotted
assignment target (`user.email=x`) is now also a static error — the lexer's
shared `Ident` token admits `.` for other legitimate uses (filenames,
`source foo.kai`), so the restriction lives in the validator, not a regex
change, with the fix (`user[email]=x`) in the message.

**`push` ships bareword-target only; bracket-path `push` is a real, tracked
gap, not a rejection.** `push services[web][tags] item` doesn't work: the
target isn't followed by `=`, so the lvalue lexer's `followed_by_eq` trigger
never fires, and the whole subscripted target fuses into a `GlobWord` that
glob-expands (failing loudly as "no matches") before `push` ever runs. Loud,
not silently wrong — but the feature the design doc originally scoped for
`push` isn't there yet. Filed in `docs/issues.md` P3 rather than attempted
here; it needs its own lexer/parser pass since the lexer has no notion of
"this bareword run is `push`'s target."

## Collection literals + spread land (2026-07-02)

`xs=[a b c]`, `{port: 8080}`/`{port:8080}`, multi-line records with a trailing
comma, nesting, and `[...$xs date]` spread — the construction half of the
collections effort, on top of the read-side resolver and the value/argv
grammar seam (both already landed). Value model unchanged: a literal just
evaluates to `Value::Json(Array|Object)`, so every existing access/`keys`/
`values`/membership/shape-guard/`${#…}` mechanism works on a literal for free.

The load-bearing decision was **context-aware lexer suppression over a global
rule**. The obvious fix — stop the glob-merge pass from ever fusing a
`[`-leading run — was rejected up front: kaish's argv glob path (`ls [dog]`,
`foo[0-9]`) and scalar assignment (`x=foo:bar`) both lean on the *existing*
fusion behavior, so a global change would either break those or need a second,
parallel "argv assembles from primitives" grammar that doesn't exist. Instead
`lexer::compute_value_context` walks the token stream once and marks a
per-token flag — inside/opening a value-position `[`/`{` literal — and the
glob-merge/colon-merge passes skip fusion *only* where that flag is set. Value
position starts right after `Token::Eq` (assignment) or a genuine membership
`Token::In`; everything else is untouched.

The load-bearing subtlety is that the two tokens the suppression keys on are
each **reused** for a non-value grammatical role, and getting the split wrong
breaks real syntax. This took three iterations to get right (two intermediate
heuristics landed and regressed before the grammar-exact rule) — the story is
worth keeping because it's a clean case of "reason from the grammar, not the
token":

- **`Token::In` is both membership and a statement head; `Token::Eq` is both
  assignment and `[[ ]]` comparison.** Membership `in` (`[[ e in $c ]]`) must
  open value position for its RHS literal; a `for`/`case` head `in`
  (`for f in *.txt`, `case 5 in [0-9]*)`) must NOT (the following word is an
  argv glob / char-class pattern). Symmetrically, an assignment `=`
  (`x=[a b]`) must open value position; a comparison `=` (`[[ $x = [0-9]* ]]`)
  must NOT. The **grammar-exact discriminator is `[[ ]]` test depth**:
  membership `in` occurs only *inside* `[[ ]]`, a head `in` only *outside*;
  assignment `=` occurs *outside*, comparison `=` only *inside*. So
  `compute_value_context` tracks `test_depth` in its forward pass and opens
  value position on `Token::In` iff `test_depth > 0` and on `Token::Eq` iff
  `test_depth == 0`. (`==` is a distinct `Token::EqEq` and never opens value.)
  Two rejected intermediate heuristics that each shipped and regressed:
  (1) a fixed three-token lookback `For Ident In` — covered `for` but missed
  `case`'s variable-length head (`case EXPR in`), so `case 5 in [0-9]*)`
  parse-errored; (2) a general pending-`for`/`case` counter — fixed the `in`
  half but the `=` half was still wrong (`[[ $x = [0-9]* ]]` regressed), and a
  bareword `for`/`case`/`in` inside `$(…)` could leak the counter/flag past
  `RParen`. The `test_depth` rule subsumes all of it: no per-keyword casing, no
  state to leak across `$()` (a bareword `in` inside `$(echo in)` is at
  `test_depth 0` → never membership; a real sub-shell assignment `$(x=…)` is
  also `test_depth 0` → still an assignment), and the `[[` detection is guarded
  by `!currently_value` so a glued nested value list `x=[[a] [b]]` reads as
  literal brackets, not a bogus test.
- **A pure `Star`/`Question` glob at value position must NOT suppress.**
  The first pass suppressed *any* run that opened right after `Eq`, which
  broke the existing `X=*.txt` → literal-string-`"*.txt"` invariant (caught by
  the pre-existing `kernel::tests::test_glob_in_assignment_is_literal`, not a
  new test — the full suite is the safety net here). Narrowed to: suppress
  only when the run actually contains an `LBracket`/`RBracket` pair — a glob
  with no brackets at value position is unaffected and keeps evaluating to a
  literal string exactly as before literals existed.

The colon-fusion exemption for `{port:8080}` needed the same care in the other
direction: it must NOT fire for a plain scalar assignment like `x=foo:bar`
(which must keep fusing into one `Ident`), so the suppression flag there is
narrower than the bracket one — specifically "inside an open value-position
`{`", not just "at value position".

AST: `Expr::ListLiteral(Vec<ListElem>)` / `Expr::RecordLiteral(Vec<RecordEntry>)`,
`ListElem::{Item,Spread}`, `RecordKey::{Bare,Quoted}`. Eval landed twice (async
`kernel.rs::eval_expr_async` for real execution, sync `interpreter/eval.rs`
`Evaluator::eval` for the reduced sync path) with a shared `spread_non_list_message`
helper so the two paths can't diverge on wording for a non-list spread — the
same dual-eval-site pattern as `${#…}` length and `${…:-default}` before it.

Deferred (`docs/issues.md` P3): deeply-nested *glued* list literals
(`x=[[a] [b]]` — spaced nesting works fine, only the glued form isn't
unfused), and the multi-word-bareword-record-value error
(`{msg: hello world}`) is loud but carries chumsky's generic message instead
of the hand-crafted "quote it" wording the design doc sketched.

## Importance-ranked onboarding tiers (2026-07-01)

Step 3 of the collections effort splits in two: the tier *mechanism* (pure infra,
this entry) and the collections *fragments* (which describe not-yet-built syntax,
so they wait on a ship-vs-panel scoping call). The mechanism landed first because
issues.md wanted it "before the collections fragments land — they're the feature
that will test it."

The always-on instruction block an embedder ships was an undifferentiated blob in
registry order; nothing capped its size and nothing guaranteed the load-bearing
rules came first. Now `Fragment` carries an importance `rank` (0 = most important;
`UNRANKED` default keeps registry order), and `select_for_concept` stable-sorts by
it — a deliberate no-op for any all-`UNRANKED` concept, so `syntax.md`
(`render_syntax_reference`, which doesn't even go through that path) and the REPL
welcome stay byte-identical. The ~10 Foundations Summary fragments got explicit
ranks 0..9, ordered by what an agent needs to write *correct* kaish first
(no-word-splitting, quote-to-join, then substitution/iteration, then capability
and safety guarantees, with the verbose overlay-mode trailing). A char-budget test
now keeps the spine lean.

Two implementation notes worth keeping: `const fn ranked(self, rank) -> Fragment {
Fragment { rank, ..self } }` relies on functional record update in const context
(stable since Rust 1.83; MSRV is 1.85), which let the rank attach in the static
registry without a builder-signature change touching every call site — only the
ranked fragments changed. And the sort key must stay `rank` alone: adding `key`
would alphabetize equal-rank fragments and reorder the Model concept the REPL
welcome depends on. Deepseek review confirmed the no-op-for-unranked claim and
flagged one latent case (Reference-depth Foundations would separate a Contrast
from its Rule — inert today, filed P4) and the public-field break (loud compile
error, marked BREAKING).

## Native collection read access: the bracket path resolver (2026-07-01)

Step 2 of the collections effort — reading into a value with `${xs[0]}`,
`${r[key]}`, `${r[$k]}`, `${r["weird-key"]}`, `${xs[-1]}`, `${xs[0:2]}`, chained
`${a[b][c]}`, and `${#…}` list/record length. Deliberately *before* the
literal-grammar gate: it only consumes `${…}` segments the lexer already splits,
so no value/argv bifurcation and no glob-merge change — the read side was
"half-built" and this finishes it.

A reconnaissance pass (Explore agent) charted the machinery and surfaced the
shape of the work: the lexer already hands out `["VAR","[0]","[k]"]`, but
`parse_varpath` was *filtering every bracket segment out* (so `${x[0]}` silently
returned the whole value — a latent silent bug the red test baseline caught
immediately), and `resolve_path` rejected anything multi-segment. The clean
insight that shaped the design: dynamic keys are just `$var`, resolved through
`scope.get`, and `resolve_path` is a `Scope` method — so the **entire traversal,
dynamic keys included, lives in `resolve_path`**, and all four var-ref eval sites
(sync + async, expression + string) get collection access for free with no
evaluator plumbing.

The load-bearing decision was the error channel. `resolve_path` went from
`Option<Value>` to `Result<Value, PathError>` with a two-variant split that the
whole no-silent-fallback directive rides on: `UndefinedRoot` is **soft** (an
error in expression position, empty inside a string — bash-compatible, preserving
every existing `"${UNSET}"` → empty), while `Invalid` (subscript on a scalar,
out-of-bounds, missing key, dotted access, string-key-on-list, integer-index-on-
record) is **loud everywhere, including inside double-quoted strings** — never
swallowed to empty. That distinction is exactly why the four primary eval sites
each match both arms explicitly; the three reduced *sync* pipeline sites keep
their pre-existing best-effort coalescing (a known narrow gap, filed P3).

Access unwraps at the boundary through `json_to_value_no_envelope` (the same
envelope-free law `fromjson` uses — an envelope-shaped sub-object stays a record,
never becomes bytes), so `[[ ${cfg[healthy]} == false ]]` is a typed bool compare
and `$(( ${cfg[port]} + 1 ))` just works — the latter needed teaching the
arithmetic mini-parser's `${…}` branch to collect bracket runs and reuse
`parse_varpath` + `resolve_path` rather than choking at the first `[`.

Brackets-only is enforced by making a dotted `${u.name}` a loud error suggesting
the bracket form (the lexer still splits `.field`, so a non-root `Field` segment
*is* the dotted case). Semantic calls, all consistent with the loud-errors ethos:
missing key errors (not bash's silent empty — `[[ k in $r ]]` is the presence
test), integer index on a record errors ("integers index lists"), a dynamic
string key works as a list index if it parses as one. Deepseek review confirmed
the core (envelope-free at every level, panic-free bounds math, correct arithmetic
bracket balancing) and turned up only the two low-priority follow-ups now in
issues.md. Twenty-four kernel-routed tests, built on `fromjson` to construct the
values the literal grammar can't yet.

## JSON bridge: fromjson / tojson land ahead of the grammar (2026-07-01)

First implementation step of the collections effort, and deliberately the one
that touches no lexer or parser. `fromjson`/`tojson` are the value model's text
boundary — the pair the arrays-and-hashes design doc (since retired into
`docs/designing-syntax-with-llms.md`) sketched as "prototype early." They
exercise the whole value-model plumbing (structured `$()`, `.data`, typed
positionals, assignment capture) end to end, so building them first de-risks the
boundary semantics and pins the error copy before the big grammar PR.

Two integration points were verified against HEAD before writing a line, because
they decide whether the pair is useful *before* native access exists: (1)
command-substitution already prefers a result's `.data` (`kernel.rs:3716`), so
`cfg=$(… | fromjson)` captures the structured `Value`; (2) a variable holding a
`Value::Json` expands straight into `args.positional` as that typed value
(`kernel.rs:3285`), so `tojson $cfg` reads a real value off the positional, not a
stringified copy. Both held — the pair works today, with access arriving later.

The load-bearing decision is **envelope-free conversion**. The internal
`json_to_value` auto-decodes any object matching the base64 byte-envelope shape
into `Value::Bytes` — correct for internal round-tripping, a silent-corruption
trap for *external* JSON that happens to match. Added
`json_to_value_no_envelope` (kaish-types) and routed `fromjson` through it; the
hazard is pinned by a test that feeds an envelope-shaped document through
`fromjson | tojson` and asserts it re-serializes as a record, not as refused
binary. `fromjson` is one-doc-one-value (serde's trailing-garbage rejection does
the work; empty input is a separate loud error), and its parse errors carry
serde's line:column. `tojson` is text-out-only — setting `.data` would make
`$(tojson $x)` round-trip straight back to a value and defeat the export escape
hatch — and refuses `Value::Bytes` loudly rather than emit an envelope. The
roundtrip law (`fromjson "$(tojson $x)"` ≡ `$x`) is test-pinned. Both are pure
data: registered unconditionally, verified under `--no-default-features`, and
tested through `KernelConfig::isolated()` (no localfs).

## Arrays & hashes design revision: the brackets are ours (2026-07-01)

The 2026-06-05 collections proposal came back for review a month later and left
substantially changed — not because the method was wrong (the weak-model stress
tests held up), but because grounding it against the current code surfaced two
facts that flipped decisions, and a design conversation with Amy pushed one
principle — consistency over convenience — further than the original draft dared.

Three Sonnet exploration passes re-grounded every file:line claim (several had
drifted; two issues.md citations pointed at entries that no longer exist). Two
discoveries did real work. First: `for x in a b c` word lists already parse, so
the proposal's bare-builtin for-head (`for k in keys $r`) wouldn't have filled a
grammar hole — it would have changed the meaning of valid syntax, with the
quasi-reserved word set growing every time a builtin ships. Replaced by
`for k in $record` iterating keys: Python's exact prior, zero grammar cost.
Second: the lexer's `Ident` regex admits `.`, so `user.email=x` parses *today* as
a flat, unreadable variable — dotted path-writes would have silently diverged
from that. That discovery sealed Amy's instinct to drop dotted access entirely:
**brackets only** (`${user[name]}`, `${r[$k]}`, `${r["weird key"]}`), bareword
subscripts are literal keys, jq keeps the dot-shaped language so the two surfaces
never blur. The doc keeps the superseded evidence annotated rather than erased —
the record of *why* is part of the method.

The review also filled gaps the original never addressed: scalar unwrap at the
access boundary (so `[[ ${cfg[healthy]} == false ]]` is a typed comparison, not a
stringify accident), structural equality collection↔collection with a loud error
for collection↔scalar, and a full lvalue ruleset — bracket paths, no
autovivification, in-bounds index set, POSIX-name LHS tightening. A fresh lexer
collision fell out of writing the implementation notes: glued `[]`/`[dog]`/`[1]`
merge into GlobWords today (`x=[dog]` is currently a *glob assignment*), so a
`[`-leading token at value position is now ALWAYS a list literal — a small
breaking change Amy ratified with "we own the []". limits.md's "kaish will never
have `[]` array syntax" line got the falsehood treatment the same session.

Cross-model review, house style (no diff, whole files): a gemini-pro batch with
the lexer and parser attached found what the doc's impl notes underestimated —
kaish's pre-parser token-fusion passes are hostile to the new literals.
`merge_colon_adjacent` eats unspaced record colons (`{port:8080}`), the
double-quote regex can't survive quoted keys inside interpolated strings, and the
shared expression parser must bifurcate into value/argv grammars or `ls [dog]`
grows a JSON argument. Every lexer claim was verified locally before folding.
Gemini also pushed back on `push` being top-level-only — the nested-append
workaround (`a[b]=[...${a[b]} new]`) was hostile enough that Amy reversed it:
push takes bracket paths under the lvalue rules. Unspaced colons: accepted.
Quoted keys in strings: loud error suggesting assign-first. A deepseek second
pass on the post-fold doc was still in flight when this PR opened.

Two forward commitments landed in the doc. `fromjson`/`tojson` (jq-named, pure
data, envelope-FREE conversion — `json_to_value`'s bytes-envelope sniffing must
never run on external JSON) get prototyped *before* the grammar: they exercise
the whole value-model plumbing without touching the parser. And the help system
grows importance-ranked ~200–300-char onboarding tiers (Amy's design), with
`help collections` as a fragment query rather than a file, and the panel re-test
gated on testing the *composed* help artifact — the thing we ship, not an ad-hoc
cheat sheet. Collections grammar work does not start until that re-test passes.

## `classify_command`: a supported command classifier for embedder preflight (2026-06-30)

Follow-on to the typed latch accessor (#45). kaijutsu wants to gate kaish's
destructive/external operations for consent — walk a script, find the command
nodes that escape to `PATH`, and block until they're approved. It can already do
most of that with the public surface (`parser::parse`, the `ast`, `tool_schemas`,
`has_function`). The one thing it *can't* do without forking kaish's truth is
classify a command name the way the interpreter resolves it — the two rules that
decide that (`is_static_command_name`, the special-form set) were private helpers
in the validator. If kaijutsu hardcodes copies, the day kaish refines name
resolution its consent gate silently disagrees with what kaish actually runs.
That's the silent-divergence failure class, and here it's security-relevant.

So we added `Kernel::classify_command(name) -> CommandKind` (`Builtin` /
`UserTool` / `Special` / `Dynamic` / `External`, plus `escapes_kernel()` for the
two buckets a gate scrutinizes). `CommandKind` lives in `kaish-types` (pure data,
`#[non_exhaustive]` so a future variant doesn't break embedders — and the safe
default for an unknown kind is to gate it). The classification core is one shared
`classify_command_name` in `validator/walker.rs`; the kernel computes the two
booleans and delegates, so there's a single source of truth.

The sharp bit was discovered empirically: the validator's `is_special_command`
set (`true`/`false`/`:`/`readonly`/`local`) is **not** what the interpreter
actually short-circuits. At runtime `readonly` resolves to an *external* command
(exit 127), `:` is a parse error, `local` is parser-level. The validator uses that
broad set only to suppress a "command not found" warning. `classify_command`
deliberately mirrors the **interpreter's** real special-forms
(`true`/`false`/`source`/`.`) instead — a consent gate must see `readonly` as
`External`, not be told it's internal. The validator↔runtime mismatch is now
filed as a P3 (the validator probably shouldn't suppress those warnings).

A deepseek review (kaibo) caught the one real under-reporting hole: execution
expands **aliases** before the registry/PATH lookup, so `alias cat=/bin/evil`
would have classified as `Builtin` while running external — the dangerous
direction for a consent gate, and there was no public alias API for the embedder
to compensate. `classify_command` now expands aliases internally, mirroring
`execute_command_depth`'s bounded recursion (special-forms re-checked each step),
so it reports `External` there too. `/v/bin/cat` and `.kai`/backend tools still
over-report as `External` (the safe direction — over-gate, never leak a `PATH`
escape). Added a `${VAR}` → `Dynamic` guard for the string-API surface.

**Staying in sync with the executor.** The classifier duplicates the
interpreter's resolution rules, which is the same silent-divergence risk the
feature exists to kill — just moved inside the kernel. Three guards keep it
honest: (1) *membership* never drifts because classify reads the live registry
and user-tool tables, not a copy; (2) the *special-form set* is a single
`SpecialForm` enum whose `from_name` is the only place a name becomes "special" —
classify reports it via `is_runtime_special_form`, and `execute_command_depth`
matches the enum **exhaustively**, so adding a form is a *compile error* until
both the name mapping and the behavior are updated (the first cut used a const +
`unreachable!`, but a deepseek round-2 review flagged that as a runtime panic the
drift test didn't actually exercise — the enum makes it compile-enforced and
drops the panic); (3) a `classify_command_matches_executor` drift test pins the
rules that *can't* be compile-enforced (user-vs-builtin precedence, alias
expansion) by classifying *and* observing the real resolution for each kind, and
now executes every special form (incl. `source`/`.`), not just `true`/`false`.
Add a resolution step to the executor without teaching classify, and either it
won't compile or that test fails.

Deferred, same reasoning as the `kaish-edit` crate: the `PreflightReport`, the AST
walk, and the consent loop are embedder policy (kaijutsu owns them), and a
`Kernel::preflight(src)` convenience waits for a second consumer. Docs: a
"Preflighting a script for external commands" recipe in EMBEDDING.md.

## Interpreter stack-depth analysis → first GitHub Issues (2026-06-30)

A question — "what pushes up the interpreter's need for stack space over time?" —
turned into a map of the async statement engine and three filed issues. The shape
of the answer is worth keeping: stack cost here is **call-chain depth × fat async
frames**, not data.

- The real statement engine is the async kernel in `kernel.rs` (not the sync
  `Evaluator` in `interpreter/eval.rs`, which only does arithmetic/test
  sub-eval). Its core is a set of mutually-recursive `Box::pin`-returning
  functions — `execute_stmt_flow`, `eval_expr_async`, `eval_string_part(s)_async`,
  `eval_test_async`. Rust async recursion *requires* boxing, and each boxed level
  keeps every live local across every `.await`, so frames are heavy.
- The deepest cycle is command substitution: `Expr::CommandSubst(Vec<Stmt>)`
  re-enters `execute_block_capturing` → `execute_stmt_flow`, stacking two boxed
  futures per `$( … )` nesting level. Recursive shell functions
  (`execute_user_tool`) and `.kai`→`.kai` sourcing (`try_execute_script`) deepen
  the *same* thread stack the same way. None of these is depth-guarded — only
  alias re-entry (`<10`) and the lexer (`MAX_PAREN_DEPTH = 256`) are.
- Per command, the dispatch chain (`execute_pipeline` → runner → `dispatch_command`
  → `execute_command_depth`) carries a ~30-field `ExecContext` that's copied twice
  in `dispatch_command`, plus `#[tracing::instrument]` future bloat on the hot
  frames.
- Runtimes are plain `Runtime::new()` (no custom stack). The foreground root future
  runs on the `block_on` thread; forked work (pipeline stages, scatter, background)
  hops to tokio worker threads with the default ~2 MB stack — so the same script
  has less headroom inside a pipe than in the foreground.

Filed as [#46](https://github.com/tobert/kaish/issues/46) (depth guard — a loud
error beats a silent `SIGSEGV`), [#47](https://github.com/tobert/kaish/issues/47)
(explicit, uniform worker-thread stack size), and
[#48](https://github.com/tobert/kaish/issues/48) (a profile-first memory/allocation
pass — suspected low cost with occasional peaks, not urgent). These are the first
issues we've put on GitHub instead of [issues.md](issues.md): an experiment ahead of
announcing kaibo, where outside agents and people will want a public tracker.
`devlog.md` stays in-repo and ships with the code — these repos also teach how a
project gets sculpted.

## `execute_argv` — argv-native kernel entry point (landed 2026-06-29)

`execute(&str)` is *string-native*: it lexes and parses its input. A caller that
already holds tokenized argv — a structured embedder (kaijutsu), a future
busybox-style multicall binary — had to re-quote argv into a string just so the
lexer could split it apart again, a round-trip that's *lossy* for typed values
(`to_argv()` stringifies `Bytes`/`Json`). `Kernel::execute_argv(name, &[Value])`
is the peer door that skips it. Full design: [multicall.md](multicall.md).

**The implementation deviates from the design doc's letter, deliberately.** The
doc proposed a new `build_args_from_argv` that builds `ToolArgs` directly,
"mirroring `build_args_async` minus the `Expr` eval." Writing a *second* binder is
exactly the drift hazard the validation-builder unification (2026-06-23) was about.
Instead the only new logic is `argv_to_args(&[Value]) -> Vec<Arg>`: a classifier
that maps each token to the AST `Arg` the lexer would produce for the equivalent
minimally-quoted word (`--` → `DoubleDash`, `-x…` with an alpha lead → `ShortFlag`,
`--k=v` → `Named`, identifier `k=v` → `WordAssign`, else literal `Positional`; a
**non-string** `Value` is always a literal positional — the typed-passthrough win).
`execute_argv` then runs the resulting `Command` through `execute_pipeline`,
*reusing the entire string-door dispatch chain verbatim* — command resolution,
arg binding, `--json`, the latch. Two binders can't drift because there's still
only one. Typed values survive because `Expr::Literal(Value)` carries the value
and evaluates by identity (it never becomes a `GlobPattern`/`VarRef`, so no
glob/`$VAR`/split can occur).

**Semantics:** tokens are literal — the "single-quoted word" rule taken to its end.
`execute_argv("echo", &["*.txt"])` emits `*.txt`. The kernel's pre-execution
*syntax* validator doesn't run (argv has no shell syntax; a tool's own
`validate()`/clap parse still does, at dispatch).

**Tests** (the design's four surfaces): an in-crate classifier suite incl. the
workspace's **first proptest** — `classifier_matches_parser_on_clean_tokens`
asserts `argv_to_args` agrees with the *real parser* on metachar-free tokens. It
earned its keep immediately, shrinking to `:A=0`: the lexer colon-merges `:A` into
one `Ident` and parses it as a `WordAssign`, where the classifier (bash-correctly)
makes a positional. They converge observably anyway — a `WordAssign` for any
non-`export`/`alias` command stringifies straight back to a `"key=value"`
positional — so the property compares *logical arguments*, not AST tags, and the
case is pinned as a regression seed. Plus kernel-routed `execute_argv_tests.rs`:
argv door ≡ string door over a corpus, literal-not-globbed, no `$VAR` interp,
typed `Int`/`Json` passthrough, `--json` transform, exit-127, and a full latch
round-trip (gate → `--confirm=<nonce>`) all through the argv door.

**Deferred:** the `kaish-multicall` binary (the cheap frontend half) and the
`&[String]` convenience door — both in issues.md.

## rg-features port — `--ftype` filtering on grep + glob (landed 2026-06-27, #38)

`rg` was dropped under the 80% rule, but its still-useful filtering re-homed onto
kaish's two *modern* search builtins. Driver: kaibo's hot path (type-scoped greps,
early-stop on match caps). The walker engine was already done — `WalkOptions.types`
had been live but dormant — so this was surface wiring plus a shared registry.

Landed scope (the design lived in the transient `search-features-port.md`, deleted
on ship):
- **grep + glob only; `find` stays POSIX** (no `--ftype` on find — it keeps
  traditional behavior; the `--no-ignore` recovery question for both search builtins
  and for find-under-`Enforced` is the live deferral, now in issues.md P3).
- **`--ftype` is the kaish-wide file-type-filter standard**, deliberately *not* rg's
  `-t`. Both grep and glob get `--ftype` / `--ftype-not` / `--ftype-list`, sharing
  one `kaish-glob::build_file_types` registry so they can't drift. An unknown type
  name is a loud exit-2, never a silent empty match.
- **All new flags are long-only, no shorts** — sidesteps GNU-grep `-T`/`-m` muscle
  memory landing on different semantics.
- grep also got **`--hidden`** (include dotfiles, bash no-dotglob default off) and
  GNU-semantics **`--max-count <N>`** (per-file cap, streaming early-stop so it
  bounds work on large/piped inputs; a truncated-at-cap UTF-8 carry is no longer
  misflagged as binary). glob keeps its fd-style `-t`/`--type` (entry *kind*:
  file/dir), which composes with the new extension-based `--ftype`.

DeepSeek-reviewed (via kaibo) on the branch.

## Correctness one-offs — grep -c exit, `$()` trim, jq /0 (landed 2026-06-24)

Three small independent silent/surprise fixes (`correctness_oneoffs_tests.rs`):

- **`grep -c` exits 1 on zero matches** (GNU). Was always exit 0 — the count is
  printed but the status must still signal "no match." Both the single-buffer and
  multi-file count paths now set `code = 1` when the total is 0 (multi-file: only
  when *no* file matched; a read-error still overrides to 2).
- **`$()` strips only trailing newlines**, not all trailing whitespace. The bare
  `Expr::CommandSubst` arms used `.trim_end()` (ate spaces/tabs); now
  `trim_end_matches('\n')` — the *exact* trim the quoted `"$(…)"` interpolation path
  (`StringPart::CommandSubst`) uses, so bare and quoted command substitution agree
  (they're the same operation; the for-loop split path's extra `\r` strip is its own
  line-splitting concern). Significant trailing spaces survive (`x=$(printf 'a  ')`).
- **jq `. / 0` fails loudly** instead of silently returning `null`. The mechanism:
  jaq evaluates `n/0` to a non-finite `Val::Float` (inf, or NaN for `0/0`), and
  `val_to_json` did `from_f64(inf) → None → unwrap_or(Null)` — a silent-wrong null.
  New `has_nonfinite_float` (recurses arrays/objects) gates the result loop and
  errors. **Decision:** this also errors on jq's `infinite`/`nan` *literals*, where
  real jq clamps `infinite` to max-f64 and renders `nan` as null. We can't tell a
  division-by-zero inf from an `infinite`-literal inf at serialization time (jaq
  doesn't error at the division like real jq does), and a loud error beats a silent
  null (the no-silent-fallbacks / crash-over-corruption directive). `infinite`/`nan`
  literals are rare; accepted divergence. The separate jaq float-formatting quirk (`3.0` for `6/2`,
  `1e10`→`10000000000.0`) is untouched — still in issues.md P3.

## Common-idiom lexer gaps — `@`, hyphenated numbers, `-1k` (landed 2026-06-24)

Three everyday agent inputs used to fragment into adjacent tokens and trip the
no-token-pasting guard — a *loud but wrong* parse error on ubiquitous text. All
fixed at the lexer with raw-slice tokens (so leading zeros survive), wired
through the parser exactly where `NumberIdent` already flows (argv/case/merge):

- **Bare `@`** — `@` is now an ordinary word char. Mid-word (`user@host`,
  `a@b.com`) joined the `Ident` trailing class; leading-`@` (`@scope/pkg`, `@0`,
  bare `@`) got a new `AtWord` token. `user@host:8080` still colon-merges into
  one word for free (Ident is colon-mergeable).
- **Digit-leading hyphenated words** (`2024-01-02`, `10-20`, `1.5-2`,
  `cut -f 1-3`) and **minus-led numeric predicate values** (`find -size -1k`,
  `-30d`) got one `DashNumWord` token (two regexes). A plain `2024`/`1.5`/`-1`
  stays `Int`/`Float` — the hyphen form requires a `-segment`, the minus form an
  alpha after the digits, so the two never overlap with `Int`/`Float`/`NumberIdent`.

**A deliberate decision reversed, intentionally.** `tr -d 0-9` was previously
made a *loud error* (and pinned by `bareword_comma_tests::numeric_range_is_loud_not_silent`)
because `0-9` could only fragment into `Int(0)`+`Int(-9)` and silently delete just
`0`. That was the best available then, not a desired end state. A contiguous `0-9`
the user typed is *one word*, not two pasted tokens — so it now reaches `tr`
verbatim and the range applies, matching bash/GNU. The test was rewritten to assert
the new behavior; the comma run (`echo 1,2,3`) stays a loud no-pasting error
(commas aren't part of the word). Strictly additive otherwise: every newly-valid
input was a parse error before, so no previously-valid program changes meaning.

## diff/patch improvements — `diff --json` + GNU-fuzz `patch` (landed 2026-06-22)

The diff/patch half of the [editing-for-agents](editing-for-agents.md) design
pass (the `edit` builtin was declined for kaish — embedder concern). Two changes,
both on branch `feat/diff-patch-improvements`:

- **`diff --json`** attaches structured hunks via `OutputData::with_rich_json`
  (same mechanism as `grep --json`) while the text node keeps serving pipes/humans.
  Built from `similar`'s `iter_hunks()` ranges + `iter_changes()` tags. The kernel
  only serializes the rich_json when `--json` is requested, so plain `diff` is
  byte-for-byte unchanged.
- **`patch` gained GNU-style offset search + fuzz**, replacing the
  stricter-than-GNU exact-context matcher that hard-failed on any drift (an
  "attractive nuisance" — agents pipe a near-miss diff, it rejects, they loop). The
  new pure `apply_hunks` locates each hunk by matching its context near the
  header position (searching outward → *offset*) and, failing exact, trimming up to
  2 context lines per end (*fuzz*); only the verified span is rewritten, so fuzz
  never overwrites unverified lines. Offset/fuzz are reported loudly (`Hunk #N
  succeeded at L (offset O lines)`); no-match still fails loud (`Hunk #N FAILED`),
  file untouched. Applied via a whole-file `PatchOp::Replace { offset:0, expected:
  Some(original) }` — TOCTOU-safe CAS, uniform across local/overlay backends.

**Decisions worth keeping:** the strict→fuzzy relaxation is *within one algorithm,
reported* — explicitly NOT the rejected "hijack patch → auto content-anchor on
failure" (a silent algorithm switch, the footgun kaish forbids). Reflex survey
showed agents reach for `patch`/`sed -i` by reflex; research showed line-numbered
diffs are the format LLMs generate worst — so `patch` stays a faithful workalike
and the reliable content-anchored path lives in the embedders (kaijutsu's hashline
`edit`), not here. The `apply_hunks` core is pure and unit-tested (clean/offset/
fuzz/no-match/reverse); execute-level tests confirm loud non-destructive failure.

## OverlayFs — copy-on-write overlay (landed 2026-06-10)

A CoW overlay filesystem composing with `LocalFs`/`MemoryFs`, for kaibo coder
workspaces, kaijutsu context forking, and kaish `--overlay` session mode. Landed
across `69c42e3`+`2a62a72` (MemoryFs lift + overlay core), `dddc85d` (accounting:
`resident_bytes`, `ByteBudget`), `6fe2225` (inspection API), and `d0e0deb` (kernel
surface: 64 MiB agent budget "vfs-memory", `MountInfo.resident_bytes` +
`kaish-mounts`, `--overlay` opt-in across REPL/embedder, `kaish-vfs` builtin
`status`/`diff`/`commit`/`reset`). Full design: [kaish-overlayfs.md](kaish-overlayfs.md).
Deliberately punted in core (per-file whiteout only, layer-local symlink
resolution, no mtime propagation in `commit_into`) — the live residuals are in
issues.md.

## Binary data & strict UTF-8 (landed 2026-06-13)

kaish was UTF-8 text end to end (`ExecResult.out: String`, string-shaped
`OutputData`, pipes consumed as text), so raw bytes couldn't transit and
`/dev/urandom` couldn't exist. Two intertwined efforts fixed that.

**Strict decodes.** The in-process text builtins used to `from_utf8_lossy`
stdin/files — silently replacing invalid bytes with `U+FFFD`, corruption that
looks like success. Replaced with a strict `ExecContext::read_stdin_to_text()`
(errors on non-UTF-8) wired through every text builtin; text tools now loud-error
on binary while byte-aware tools consume it. A same-pass bug: `accumulate_result`
folded every top-level statement via `push_out(text_out())`, lossy-decoding *any*
`Bytes` final result (so even a standalone `cat blob.bin` came back mangled) — now
concatenates raw bytes when binary is involved. A round-2 DeepSeek+Gemini review
caught more silent-corruption sites (cat as a pipeline's last stage, `$()`/block
capture, command-subst-in-string, `kaish-last`, background-job output, `for` over
`Bytes`, `dd skip*bs` overflow), all fixed with regression tests in
`sandbox_mode_tests`.

**Typed `Bytes`.** Design (nushell-style, full plan in
[binary-data.md](binary-data.md)): a `Value::Bytes`/`OutputData::Bytes` that flows
through pipes, coerces to text iff valid UTF-8 (else loud error), and renders at
the boundary (REPL hex dump, `--json`/MCP base64 envelope). All phases landed
(`e612d69` → `818a22e`): the value + boundary type (dead `Value::Blob`/`BlobRef`
deleted), byte-clean transit + redirects, `dd` + `/dev/urandom`/`/dev/random`,
byte-aware movers (cat/head -c/tail -c/base64/xxd/checksum/wc -c/tee/cmp), and
external-command capture → `Bytes` with raw stdin forwarding. North-star test
(`dd if=/dev/urandom of=/dev/null bs=1024 count=10` copies exactly 10240 bytes;
two draws have differing checksums) lives in `sandbox_mode_tests.rs`; DevFs in
`crates/kaish-vfs/src/dev.rs`. **Decisions:** no generic `encode`/`decode`
(`base64`+`xxd` already bridge; a generic pair invites a basenc format×flag
matrix); no `random` builtin (`dd if=/dev/urandom` covers it).

## Streaming file reads (landed 2026-06-14)

Scan-oriented builtins stopped reading whole files into memory. Mechanism respects
kaish-vfs's runtime-free trait (no `AsyncRead`/tokio in `Filesystem`, so WASI stays
clean): `LocalFs::read_range` does a true positional `seek + take`, and
`ExecContext::read_file_chunked` pulls a file forward in 256 KiB windows. Landed:
`wc` (`a610044`), `checksum` (`770766c`), `grep` single-file simple path
(`0fce8c4`), `cmp` lockstep two-file (`dcb806c`), `cat` single-file piped
(`e220767`). Each has a parity test against the *production* whole-buffer path —
that lesson came from a grep draft that hardcoded `byte_offset=0`/`path=null` in
`--json` and passed a text-only self-comparison. Deliberately NOT streamed
(`sort`/`uniq`/`diff`/`jq` and anything emitting structured `.data` — the
`.data`/`OutputData` channel is whole-value by design).

## Output disk-spill — read-only safety (core fix 2026-06-06)

`OutputLimitConfig` carries a runtime `SpillMode` (`Disk` | `Memory`); `Memory`
truncates in memory with no disk I/O. `Kernel::assemble` auto-forces `Memory` for
`NoLocal` mounts, and (fixed 2026-06-08) for any `with_backend` kernel — a
custom-backend kernel owns no host mounts, so a host spill is always a VFS bypass.
This closed the kaibo gap (kaibo uses `with_backend` and didn't opt into `Memory`).

## Composable help (Phases 1–3, 2026-06-06)

The `kaish-help` crate (concept fragments + `compose`/recipes + byte-stable
`get_help`) became the single source for help content. `syntax.md` is generated +
drift-tested; the REPL welcome, the `execute` tool description, and the embedder
prompt set all compose from it (no hand-rolled prose left). Phase 4 publish-half
done 2026-06-08 (`kaish-help` 0.8.0 on crates.io). Full design + resolved
decisions: [composable-help.md](composable-help.md).

## Test fortification (through 2026-06-14)

The 2026-06-09 Fable 5 systemic review (61-agent fleet) drove a fortification pass.
Closed: destructive rails, the realworld port (48 tests through `kernel.execute`,
which immediately caught the grep/rg value-flag binding bug), the `--json` sweep
with a drift guard, snapshot-isolation races, tautological asserts, external-argv
no-split, kill/wait e2e. Lexer negative tests tightened to assert exact
`LexerError` variants (`2026-06-14`), which surfaced a real diagnostic gap (an
unterminated `"string` emits the generic `UnexpectedCharacter`, not the curated
`UnterminatedString`). Seven never-emitted `IssueCode` variants were removed as
silent aspiration; E011/E006/E007 later re-added *with* real `validate` emitters,
honoring the no-variant-without-an-emitter rule.

## The Feb-2026 P0 bugs (validated fixed 2026-04-16)

The original P0 from the Feb-2026 integration-test pass — unknown-command socket
error, missing `$0`, alias concatenation, subprocess capture, arithmetic token
leak — all validated as fixed and were retired from the punch list.

---

## Standing decisions (don't re-litigate)

- **No rustfmt (2026-06-14, Amy).** The audience for this code is Claude and other
  agents, not human reviewers, so rustfmt's payoff is mostly cosmetic; the compiler
  + `cargo clippy --all --all-targets` are the real gate. A first `cargo fmt --all`
  rewrites ~202 files / ~2127 hunks and no small config shrinks it meaningfully
  (`use_small_heuristics` Off/Max both make it worse; the knobs that would preserve
  the hand-style are nightly-only). No `rustfmt.toml` is added (a dormant one makes
  ad-hoc `cargo fmt --check` fail confusingly). If ever revisited: its own commit,
  pinning `edition = "2024"`.
- **No unquoted token-pasting (2026-06-08).** A run of adjacent *unquoted* lexemes
  is never concatenated into one word — `/tmp/$(echo x).txt` lexes as three tokens.
  The quoted form (`"/tmp/$(echo x).txt"`) is the supported idiom and aligns with
  the `shellcheck --enable=all` north star (SC2086). kaish guides agents to write
  reliable scripts; "always quote interpolated words" is simpler and lint-aligned
  than bash's implicit pasting. (Live polish residuals — redirect-target hint,
  post-`--` glue — are in issues.md.)
- **No generic `encode`/`decode`; no `random` builtin (2026-06-13).** See the
  binary-data section above.
- **No `test` builtin and no `[` command (2026-06-25, Amy).** Conditionals go
  through `[[ … ]]` only. The old `test`/`[` builtins were removed: `[`
  (`Bracket`) was effectively dead (it can't be a command name — `[` lexes as
  `LBracket`, absent from `command_parser`'s `command_name`), and `test`'s
  binary-operator half (`test a = b`, `-eq`, `<`, `>`) never reached the builtin
  because `=`/`!=`/`<`/`>` lex as operator tokens the argv parser rejects in
  argument position — only unary `test -z`/`test -f` actually routed. Rather than
  do the delicate, broad-blast-radius parser change to accept those operator
  tokens as barewords (without breaking assignment `x=y`, glob char-classes
  `ls [ab]*`, or `[[ ]]`), we keep `[[ … ]]` as the single test form. **Why
  `[[ ]]` wins:** it's real grammar the validator sees, so a malformed test /
  unknown operator / unquoted expansion is caught *before* runtime; `test`/`[`
  hide their operators as runtime string args (the late-failure footgun kaish
  exists to remove). The fully-written `test`/`[` builtin (with passing
  *direct-`.execute()`* unit tests — the gotcha CLAUDE.md warns about: they
  bypassed the real lex/parse path) was the tell that this never worked end-to-end.
  **Guard (gemini-batch review of PR #29):** a bare `test` in a subprocess build
  falls through to an external `/usr/bin/test` that evaluates against the *real*
  host FS, bypassing the VFS/overlay — a silent wrong boolean into `if`/`&&`. We
  added validator advisory **W006** (`IssueCode::PosixTestCommand`) that steers to
  `[[ … ]]` and *still runs* (Amy chose warn-don't-reject over a poison stub). It's
  the first agent-surfaced validation warning: warnings were trace-only because
  every external command fires `UndefinedCommand`, so a code now opts into
  surfacing via `IssueCode::surfaces_to_agent` — the seam for the broader P4
  "did-you-mean" pass. Surfacing is dual-path: the streaming frontend gets it via a
  pre-loop `on_output` emission; `kernel.execute` reads it off the aggregate
  `result.err` (the two consumers are disjoint, so it prints exactly once each).

## Accepted risks & waived items (decided, not open work)

These were on the issues.md punch list but reached a verdict — recorded so the
deferral stays a decision, not drift. Reopen only if a real failure surfaces.

- **Non-Linux `kill` keeps PID-based signalling.** `pidfd` is Linux-only;
  elsewhere we `kill(pid, sig)` and accept the PID-reuse race for the direct
  child. Acceptable — kaish runs predominantly on Linux.
- **Process-group kill PID-reuse window.** The PG-wide kill that catches
  grandchildren goes through `killpg(pgid, sig)` — no PGID equivalent of pidfd.
  If a leader is reaped and its PGID reused before `killpg` fires, an unrelated
  group could be signalled. Mitigations (cgroup v2 `cgroup.kill`,
  `PR_SET_CHILD_SUBREAPER`) are significant complexity; deferred until a real
  failure.
- **`JobManager::spawn` busy-waits** with `std::hint::spin_loop()` for immediate
  visibility — works, wastes CPU under contention. Channel coordination would be
  cleaner; no trigger.
- **`head -n -0` / signed-zero line counts (waived).** `-0` lexes as `Int(0)`
  (sign lost at the lexer) and `line_spec` only treats a *negative* Int as
  all-but-last, so `head -n -0` prints nothing instead of the whole file. Not
  fixable without lexer changes to preserve signed zero. Obscure; waived.
- **`mktemp` random-suffix modulo bias (recorded by choice).** `byte % 36` skews
  the first four alphabet chars (~3.1% vs ~2.7%, since `256 % 36 = 4`). Negligible
  for temp suffixes; the rejection-sampling fix complicates the
  fail-loud-on-no-entropy contract.
- **`uname -v` discloses build provenance unconditionally.** Formats
  `kaish {version} ({git_hash} {build_date})` from compile-time `option_env!`; an
  embedder that sets `KAISH_GIT_HASH`/`KAISH_BUILD_DATE` fingerprints the exact
  commit even in a minimal build. Gate behind a `verbose-identity` feature only if
  a threat model cares.
- **`mktemp` entropy-failure message is unhelpful on wasm.** A
  `getrandom::fill` failure on `wasm32-wasip1` surfaces a near-empty `Display`.
  Add a `cfg!(target_arch = "wasm32")` hint if it ever matters.
- **`to_argv()` flattens a repeatable scalar array to one JSON token (no live
  trigger).** `ParamSchema.repeatable` stores repeated single-value flags as
  `named[key] = Json(Array([scalar, …]))`; `to_argv()` only splits the
  *array-of-arrays* (`consumes > 1`) shape, so a flat scalar array becomes one
  JSON-text token. `sed` is unaffected (it reads the raw `ToolArgs`); a future
  repeatable-flag builtin that trusts its clap struct after a `to_argv()`
  round-trip would see one mangled value. The real fix needs schema context in
  `to_argv` (it has none). Record-then-defer until a builtin hits it.
