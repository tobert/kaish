//! approvals — Read the approval ledger, renew requests, and decide them.
//!
//! Subcommands: list, show, log, renew, grant, deny, revoke
//! (`docs/approval-ledger.md` §D.3).
//!
//! **This is the one builtin that bridges to the approval side, and only
//! through an authority installed on the session.** `grant`, `deny`, and
//! `revoke` exit 1 without one. Everything else — `list`, `show`, `log`,
//! `renew` — works in every session, because reading is not deciding and
//! renewing is the requester's own action.

use async_trait::async_trait;
use clap::{Args, Parser};

use kaish_types::approval::{GrantTerms, RequestId, StandingId};

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{
    params_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema,
};

/// How long a grant made with no `--until` stays redeemable. Five minutes:
/// long enough for an operator to hand the key back to the agent and for the
/// agent to re-run, short enough that a forgotten grant is not a standing
/// one. Every grant is still good for exactly one successful settlement
/// (spec §A.1) — this bounds the window, not the count.
const DEFAULT_GRANT_WINDOW: std::time::Duration = std::time::Duration::from_secs(300);

/// Approvals tool: read the ledger, renew requests, and decide them.
pub struct Approvals;

/// The flat argv layer.
///
/// kaish binds `approvals <sub> [id]` with the subcommand words as
/// *positionals*, so clap sees one command with every flag declared on it and
/// the subcommand path arrives in `args.positional`. The per-subcommand
/// **schema** tree is reflected from the `Args` structs below, so `help
/// approvals grant` lists only that subcommand's flags and the model never
/// sees `--reason` advertised on `list`.
#[derive(Parser, Debug)]
#[command(name = "approvals", about = "Read the approval ledger, renew requests, and decide them")]
struct ApprovalsArgs {
    #[command(flatten)]
    global: GlobalFlags,

    #[command(flatten)]
    list: ListArgs,

    #[command(flatten)]
    log: LogArgs,

    #[command(flatten)]
    grant: GrantArgs,

    #[command(flatten)]
    deny: DenyArgs,

    /// Subcommand (`list`, `show`, `log`, `renew`, `grant`, `deny`, `revoke`)
    /// and the request id it acts on. Read off `args.positional`; this field
    /// exists so clap accepts the tail.
    #[arg(hide = true)]
    args: Vec<String>,
}

#[derive(Args, Debug)]
struct ListArgs {
    /// List only requests still awaiting a decision. The default.
    #[arg(long = "pending")]
    pending: bool,

    /// List every request the ledger still holds, decided ones included.
    #[arg(long = "all")]
    all: bool,

    /// List the live standing grants instead of requests.
    #[arg(long = "standing")]
    standing: bool,
}

#[derive(Args, Debug)]
struct LogArgs {
    /// Show only entries after this sequence number. Pass the last `SEQ` you
    /// read to page forward without re-reading the tail.
    #[arg(long = "since")]
    since: Option<u64>,
}

#[derive(Args, Debug)]
struct GrantArgs {
    /// How long the grant stays redeemable: `30s`, `5m`, `1h`. Defaults to
    /// 5m. The grant is good for one successful settlement either way.
    #[arg(long = "until")]
    until: Option<String>,
}

#[derive(Args, Debug)]
struct DenyArgs {
    /// Why the request was denied. Recorded on the ledger entry and shown to
    /// whoever asked.
    #[arg(long = "reason")]
    reason: Option<String>,
}

/// Reflect one subcommand's `Args` struct into a child schema.
fn child<A: Args>(name: &'static str, about: &'static str) -> ToolSchema {
    let command = A::augment_args(clap::Command::new(name).about(about));
    let mut schema = ToolSchema::new(name, about);
    for param in params_from_clap(&command) {
        schema = schema.param(param);
    }
    schema
}

#[async_trait]
impl Tool for Approvals {
    fn name(&self) -> &str {
        "approvals"
    }

    fn schema(&self) -> ToolSchema {
        let mut schema = ToolSchema::new(
            "approvals",
            "Read the approval ledger, renew requests, and decide them",
        );
        for (description, code) in [
            ("List requests awaiting a decision", "approvals list"),
            ("List every request the ledger holds", "approvals list --all"),
            ("Show one request and its attempts", "approvals show req_9c1a4f2e_42"),
            ("Read the log from the start", "approvals log"),
            ("Read the log after sequence 120", "approvals log --since 120"),
            ("Re-raise an expired request", "approvals renew req_9c1a4f2e_42"),
            ("Approve a request for 5 minutes", "approvals grant req_9c1a4f2e_42"),
            ("Approve a request for an hour", "approvals grant req_9c1a4f2e_42 --until 1h"),
            ("Refuse a request", "approvals deny req_9c1a4f2e_42 --reason 'wrong path'"),
            ("Retire a standing grant", "approvals revoke 3"),
        ] {
            schema = schema.example(description, code);
        }
        schema
            .subcommand(child::<ListArgs>(
                "list",
                "List approval requests, or the standing grants that auto-approve them",
            ))
            .subcommand(ToolSchema::new(
                "show",
                "Show one request: what was asked, what was decided, and every attempt",
            ))
            .subcommand(child::<LogArgs>(
                "log",
                "Print the retained ledger entries, oldest first",
            ))
            .subcommand(ToolSchema::new(
                "renew",
                "Re-raise an expired request as a new one linked to it. Needs no authority",
            ))
            .subcommand(child::<GrantArgs>(
                "grant",
                "Approve a request. Requires approval authority on this session",
            ))
            .subcommand(child::<DenyArgs>(
                "deny",
                "Refuse a request. Requires approval authority on this session",
            ))
            .subcommand(ToolSchema::new(
                "revoke",
                "Retire a standing grant. Requires approval authority on this session",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("approvals: {e}")),
        };
        let parsed = match ApprovalsArgs::try_parse_from(
            std::iter::once("approvals".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("approvals: {e}")),
        };
        parsed.global.apply(ctx);

        let Some(subcommand) = args.get_string("subcommand", 0) else {
            return ExecResult::failure(
                2,
                "approvals: name a subcommand: list, show, log, renew, grant, deny, revoke",
            );
        };

        match subcommand.as_str() {
            "list" => cmd_list(&parsed.list, ctx),
            "show" => cmd_show(&args, ctx),
            "log" => cmd_log(&parsed.log, ctx),
            "renew" => cmd_renew(&args, ctx).await,
            "grant" => cmd_grant(&args, &parsed.grant, ctx).await,
            "deny" => cmd_deny(&args, &parsed.deny, ctx).await,
            "revoke" => cmd_revoke(&args, ctx).await,
            other => ExecResult::failure(
                2,
                format!(
                    "approvals: unknown subcommand {other:?} — use list, show, log, renew, \
                     grant, deny, or revoke"
                ),
            ),
        }
    }
}

/// The ledger this session reads and posts to, or the failure that says it
/// has none. An `ExecContext` without one is a bare unit-test harness, not a
/// kernel-built session.
#[allow(clippy::result_large_err)]
fn ledger(ctx: &ExecContext) -> Result<&crate::tools::LedgerAccess, ExecResult> {
    ctx.ledger_access
        .as_ref()
        .ok_or_else(|| ExecResult::failure(1, "approvals: this session has no approval ledger"))
}

/// This session's approval authority, or the exit-1 refusal that names why
/// there is none.
///
/// **The single most important check in this builtin.** An agent whose job is
/// running shell commands can see what is pending and renew it; it cannot
/// approve itself. Anything else makes the separation theater (spec §D.3).
#[allow(clippy::result_large_err)]
fn authority<'a>(
    ctx: &'a ExecContext,
    subcommand: &str,
) -> Result<&'a crate::ledger::ApproverHandle, ExecResult> {
    ledger(ctx)?.session_authority.as_ref().ok_or_else(|| {
        ExecResult::failure(
            1,
            format!(
                "approvals {subcommand}: this session holds no approval authority — an operator \
                 must decide it. Read what is pending with `approvals list`, and re-raise an \
                 expired request with `approvals renew <id>`."
            ),
        )
    })
}

/// The request id positional, parsed. Full-form only — there is no short form
/// (spec §A.2), so an id can never be ambiguous between sessions sharing a
/// ledger.
#[allow(clippy::result_large_err)]
fn request_id(args: &ToolArgs, subcommand: &str) -> Result<RequestId, ExecResult> {
    let Some(raw) = args.get_string("id", 1) else {
        return Err(ExecResult::failure(
            2,
            format!("approvals {subcommand}: name a request id, e.g. req_9c1a4f2e_42"),
        ));
    };
    RequestId::parse(&raw).map_err(|e| {
        ExecResult::failure(
            2,
            format!("approvals {subcommand}: {e} — ids are full-form, e.g. req_9c1a4f2e_42"),
        )
    })
}

fn cmd_list(flags: &ListArgs, ctx: &ExecContext) -> ExecResult {
    let access = match ledger(ctx) {
        Ok(a) => a,
        Err(e) => return e,
    };

    if flags.standing {
        if flags.pending || flags.all {
            return ExecResult::failure(
                2,
                "approvals list: --standing lists rules, not requests — drop --pending/--all",
            );
        }
        let standing = access.approvals.standing();
        if standing.is_empty() {
            return empty_list("(no standing grants)\n");
        }
        let nodes: Vec<OutputNode> = standing
            .iter()
            .map(|rule| {
                OutputNode::new(rule.id.to_string()).with_cells(vec![
                    rule.operations.iter().map(ToString::to_string).collect::<Vec<_>>().join(","),
                    match rule.max_uses {
                        Some(n) => n.to_string(),
                        None => "unlimited".to_string(),
                    },
                    rule.issued_by.id.clone(),
                    rule.reason.clone(),
                ])
            })
            .collect();
        return ExecResult::with_output(
            OutputData::table(
                vec![
                    "ID".to_string(),
                    "OPERATIONS".to_string(),
                    "MAX_USES".to_string(),
                    "ISSUED_BY".to_string(),
                    "REASON".to_string(),
                ],
                nodes,
            )
            .with_rich_json(json_or_null(&standing)),
        );
    }

    if flags.pending && flags.all {
        return ExecResult::failure(
            2,
            "approvals list: --pending and --all ask for different sets — pick one",
        );
    }

    // Pending is the default: the set an operator has to act on.
    let views = if flags.all {
        access
            .approvals
            .ids()
            .into_iter()
            .filter_map(|id| access.approvals.get(&id).map(|chain| (chain.state, chain.request)))
            .collect::<Vec<_>>()
    } else {
        access
            .approvals
            .pending()
            .into_iter()
            .map(|view| (kaish_types::approval::RequestState::Requested, view))
            .collect()
    };

    if views.is_empty() {
        let what = if flags.all { "requests" } else { "pending approvals" };
        return empty_list(format!("(no {what})\n"));
    }

    let nodes: Vec<OutputNode> = views
        .iter()
        .map(|(state, view)| {
            OutputNode::new(view.id.as_str()).with_cells(vec![
                state_word(*state),
                view.operation.to_string(),
                view.principal.id.clone(),
                resource_summary(view),
            ])
        })
        .collect();
    let rows: Vec<_> = views.iter().map(|(_, view)| view).collect();
    ExecResult::with_output(
        OutputData::table(
            vec![
                "ID".to_string(),
                "STATE".to_string(),
                "OPERATION".to_string(),
                "PRINCIPAL".to_string(),
                "RESOURCES".to_string(),
            ],
            nodes,
        )
        .with_rich_json(json_or_null(&rows)),
    )
}

fn cmd_show(args: &ToolArgs, ctx: &ExecContext) -> ExecResult {
    let access = match ledger(ctx) {
        Ok(a) => a,
        Err(e) => return e,
    };
    let id = match request_id(args, "show") {
        Ok(id) => id,
        Err(e) => return e,
    };
    let Some(chain) = access.approvals.get(&id) else {
        return ExecResult::failure(1, format!("approvals show: no request {id} in this ledger"));
    };

    let mut text = String::new();
    text.push_str(&format!("{}  {}\n", chain.request.id, state_word(chain.state)));
    text.push_str(&format!("  operation  {}\n", chain.request.operation));
    text.push_str(&format!("  risk       {:?}\n", chain.request.risk));
    text.push_str(&format!("  principal  {}\n", chain.request.principal.id));
    text.push_str(&format!("  reason     {}\n", chain.request.reason));
    for resource in &chain.request.resources {
        text.push_str(&format!("  resource   {}:{}\n", resource.kind, resource.id));
    }
    if let Some(job_id) = chain.request.job_id {
        text.push_str(&format!("  job        /v/jobs/{job_id}/\n"));
    }
    if let Some(superseded) = &chain.request.supersedes {
        text.push_str(&format!("  supersedes {superseded}\n"));
    }
    match &chain.grant {
        Some(grant) => {
            text.push_str(&format!(
                "  granted by {} on {:?}\n",
                grant.decided_by.id, grant.grounds
            ));
        }
        None => text.push_str("  granted    no\n"),
    }
    if chain.attempts.is_empty() {
        text.push_str("  attempts   none\n");
    }
    for attempt in &chain.attempts {
        text.push_str(&format!(
            "  attempt {}  {}  {}\n",
            attempt.attempt.get(),
            serde_word(&attempt.state),
            match &attempt.outcome {
                Some(outcome) => format!("{outcome:?}"),
                None => "in flight".to_string(),
            }
        ));
    }

    let rich = serde_json::json!({
        "request": chain.request,
        "state": chain.state,
        "grant": chain.grant,
        "attempts": chain.attempts.iter().map(|a| serde_json::json!({
            "attempt": a.attempt,
            "state": a.state,
            "outcome": a.outcome,
        })).collect::<Vec<_>>(),
    });
    ExecResult::with_output_and_text(OutputData::text(text.clone()).with_rich_json(rich), text)
}

fn cmd_log(flags: &LogArgs, ctx: &ExecContext) -> ExecResult {
    let access = match ledger(ctx) {
        Ok(a) => a,
        Err(e) => return e,
    };
    let records = access.approvals.log(flags.since.unwrap_or(0));
    if records.is_empty() {
        return empty_list("(no ledger entries)\n");
    }

    // The rich payload is the whole versioned record (spec §A.5) — the
    // `schema_version` and the scope are what tell a reader whose entry this
    // is and whether it understands it. The table reads the entry inside.
    let rows: Vec<serde_json::Value> = records.iter().map(json_value).collect();
    let nodes: Vec<OutputNode> = records
        .iter()
        .zip(&rows)
        .map(|(record, row)| {
            let entry = row.get("entry");
            OutputNode::new(record.sequence.to_string()).with_cells(vec![
                entry
                    .and_then(|v| v.get("entry"))
                    .and_then(|v| v.as_str())
                    .unwrap_or("?")
                    .to_string(),
                entry
                    .and_then(|v| v.get("request"))
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string(),
            ])
        })
        .collect();
    ExecResult::with_output(
        OutputData::table(
            vec!["SEQ".to_string(), "ENTRY".to_string(), "REQUEST".to_string()],
            nodes,
        )
        .with_rich_json(serde_json::Value::Array(rows)),
    )
}

async fn cmd_renew(args: &ToolArgs, ctx: &ExecContext) -> ExecResult {
    let id = match request_id(args, "renew") {
        Ok(id) => id,
        Err(e) => return e,
    };
    match ctx.renew_request(&id).await {
        Ok(renewed) => ExecResult::with_output(OutputData::text(format!(
            "{id} renewed as {renewed} — it needs a fresh decision\n"
        ))),
        Err(e) => ExecResult::failure(1, format!("approvals renew: {e}")),
    }
}

async fn cmd_grant(args: &ToolArgs, flags: &GrantArgs, ctx: &ExecContext) -> ExecResult {
    // Usage first, authority second: a mistyped id is exit 2 in every
    // session, so a caller never has to hold approval rights to learn it
    // typed the command wrong. Nothing here touches the ledger — the id is
    // checked for *shape*, not for existence.
    let id = match request_id(args, "grant") {
        Ok(id) => id,
        Err(e) => return e,
    };
    let handle = match authority(ctx, "grant") {
        Ok(h) => h,
        Err(e) => return e,
    };
    let window = match &flags.until {
        Some(raw) => match crate::duration::parse_duration(raw) {
            Some(d) => d,
            None => {
                return ExecResult::failure(
                    2,
                    format!("approvals grant: --until {raw:?} is not a duration — use 30s, 5m, or 1h"),
                )
            }
        },
        None => DEFAULT_GRANT_WINDOW,
    };
    let Some(chain) = handle.approvals_view().get(&id) else {
        return ExecResult::failure(1, format!("approvals grant: no request {id} in this ledger"));
    };
    // Terms taken verbatim from the request's own declared transitions: an
    // operator granting from the shell approves what was asked for, and the
    // ledger refuses a widening anyway (spec §A.4).
    let terms = GrantTerms::once_for_view(&chain.request, kaish_types::clock::system_now() + window);
    match handle.grant(&id, terms).await {
        Ok(()) => ExecResult::with_output(OutputData::text(format!(
            "{id} granted for {} — one successful settlement\n",
            humanize(window)
        ))),
        Err(e) => ExecResult::failure(1, format!("approvals grant: {e}")),
    }
}

async fn cmd_deny(args: &ToolArgs, flags: &DenyArgs, ctx: &ExecContext) -> ExecResult {
    // Usage first, authority second — see `cmd_grant`.
    let id = match request_id(args, "deny") {
        Ok(id) => id,
        Err(e) => return e,
    };
    let handle = match authority(ctx, "deny") {
        Ok(h) => h,
        Err(e) => return e,
    };
    let reason = flags.reason.as_deref().unwrap_or("denied from the shell");
    match handle.deny(&id, reason).await {
        Ok(()) => ExecResult::with_output(OutputData::text(format!("{id} denied: {reason}\n"))),
        Err(e) => ExecResult::failure(1, format!("approvals deny: {e}")),
    }
}

async fn cmd_revoke(args: &ToolArgs, ctx: &ExecContext) -> ExecResult {
    // Usage first, authority second — see `cmd_grant`.
    let Some(raw) = args.get_string("id", 1) else {
        return ExecResult::failure(2, "approvals revoke: name a standing grant id, e.g. 3");
    };
    let Ok(raw) = raw.parse::<u64>() else {
        return ExecResult::failure(
            2,
            format!("approvals revoke: {raw:?} is not a standing grant id — they are plain numbers"),
        );
    };
    let handle = match authority(ctx, "revoke") {
        Ok(h) => h,
        Err(e) => return e,
    };
    let id = StandingId::new(raw);
    match handle.revoke_standing(&id, "revoked from the shell").await {
        Ok(()) => ExecResult::with_output(OutputData::text(format!(
            "standing grant {id} revoked — requests already granted are unaffected\n"
        ))),
        Err(e) => ExecResult::failure(1, format!("approvals revoke: {e}")),
    }
}

/// An empty listing: `[]` under `--json`, and `text` for a reader.
///
/// A consumer that parses `approvals list --json` gets an array whether or
/// not anything is pending — "nothing yet" must not be a different shape from
/// "one request", or every caller needs a special case for the common state.
fn empty_list(text: impl Into<String>) -> ExecResult {
    let text = text.into();
    ExecResult::with_output_and_text(
        OutputData::text(text.clone()).with_rich_json(serde_json::Value::Array(Vec::new())),
        text,
    )
}

/// A request's resources, one line's worth: `path:/a/b`, comma-joined and
/// capped so the table stays a table.
fn resource_summary(view: &kaish_types::approval::ApprovalRequestView) -> String {
    const SHOWN: usize = 3;
    let mut parts: Vec<String> = view
        .resources
        .iter()
        .take(SHOWN)
        .map(|r| format!("{}:{}", r.kind, r.id))
        .collect();
    if view.resources.len() > SHOWN {
        parts.push(format!("+{} more", view.resources.len() - SHOWN));
    }
    parts.join(",")
}

/// The wire spelling of a `RequestState` — the same lowercase word
/// `/v/approvals/{id}/state` and `--json` use, so one state has one name
/// across every surface.
fn state_word(state: kaish_types::approval::RequestState) -> String {
    serde_word(&state)
}

fn serde_word<T: serde::Serialize>(value: &T) -> String {
    serde_json::to_string(value)
        .map(|s| s.trim_matches('"').to_string())
        .unwrap_or_else(|_| "?".to_string())
}

/// Serialize for a `rich_json` payload. A serialization failure yields JSON
/// `null` rather than dropping the whole result: these are plain
/// scalar/string/timestamp types, so it cannot happen in practice, and the
/// text rendering above already carries the answer.
fn json_or_null<T: serde::Serialize>(value: &T) -> serde_json::Value {
    serde_json::to_value(value).unwrap_or(serde_json::Value::Null)
}

fn json_value<T: serde::Serialize>(value: &T) -> serde_json::Value {
    json_or_null(value)
}

/// A duration, as an operator would say it.
fn humanize(d: std::time::Duration) -> String {
    let secs = d.as_secs();
    if secs % 3600 == 0 && secs > 0 {
        format!("{}h", secs / 3600)
    } else if secs % 60 == 0 && secs > 0 {
        format!("{}m", secs / 60)
    } else {
        format!("{secs}s")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_schema_names_every_subcommand() {
        let schema = Approvals.schema();
        let names: Vec<&str> = schema.subcommands.iter().map(|s| s.name.as_str()).collect();
        assert_eq!(
            names,
            vec!["list", "show", "log", "renew", "grant", "deny", "revoke"]
        );
    }

    /// The per-subcommand params must land on the right leaf: `--reason` is
    /// `deny`'s and nothing else's, or the model is told `approvals list
    /// --reason` is a thing.
    #[test]
    fn flags_land_on_the_subcommand_that_owns_them() {
        let schema = Approvals.schema();
        let param_names = |name: &str| -> Vec<String> {
            schema
                .subcommands
                .iter()
                .find(|s| s.name == name)
                .map(|s| s.params.iter().map(|p| p.name.clone()).collect())
                .unwrap_or_default()
        };
        assert_eq!(param_names("list"), vec!["pending", "all", "standing"]);
        assert_eq!(param_names("log"), vec!["since"]);
        assert_eq!(param_names("grant"), vec!["until"]);
        assert_eq!(param_names("deny"), vec!["reason"]);
        assert!(param_names("show").is_empty());
        assert!(param_names("renew").is_empty());
        assert!(param_names("revoke").is_empty());
    }

    /// Values tried for a value-taking flag, in order; the first that parses
    /// wins. `param_type` cannot pick for us — clap erases the field type, so
    /// `params_from_clap` reports `"string"` for both `--since` (a `u64`) and
    /// `--reason` (a `String`). A flag that accepts none of these is a
    /// prompt to add a sample here, not a drift failure, and the panic says
    /// so.
    const FLAG_SAMPLES: &[&str] = &["1", "5m", "x"];

    /// **Every flag the schema advertises must parse at the flat argv layer.**
    ///
    /// The two halves are declared separately on purpose (see
    /// `ApprovalsArgs`), and this is the seam between them. The drift it
    /// exists to catch: a new `Args` struct registered in `schema()` through
    /// `child::<New>()` but never `#[command(flatten)]`ed into the flat
    /// parser. The flag is then advertised to the model and accepted by
    /// pipeline validation, and the runtime parse rejects it with exit 2 on a
    /// documented spelling.
    ///
    /// Reflective, not a list: enumerating the flags by hand cannot catch a
    /// flag nobody thought to list, which is exactly the failure mode.
    #[test]
    fn every_advertised_flag_parses() {
        let schema = Approvals.schema();
        let mut checked = 0usize;

        for node in std::iter::once(&schema).chain(schema.subcommands.iter()) {
            for param in &node.params {
                if param.positional {
                    continue; // positionals ride the `--` tail, not a flag
                }
                checked += 1;
                let flag = format!("--{}", param.name);

                if crate::scheduler::is_bool_type(&param.param_type) {
                    let argv = vec!["approvals".to_string(), flag.clone()];
                    ApprovalsArgs::try_parse_from(&argv).unwrap_or_else(|e| {
                        panic!(
                            "`{flag}` is advertised on `approvals {}` but the flat parser \
                             rejects it — is its Args struct #[command(flatten)]ed into \
                             ApprovalsArgs? clap said: {e}",
                            node.name
                        )
                    });
                    continue;
                }

                // A value flag: one sample per value it consumes.
                let accepted = FLAG_SAMPLES.iter().any(|sample| {
                    let mut argv = vec!["approvals".to_string(), flag.clone()];
                    argv.extend(std::iter::repeat_n(
                        (*sample).to_string(),
                        param.consumes.max(1),
                    ));
                    ApprovalsArgs::try_parse_from(&argv).is_ok()
                });
                assert!(
                    accepted,
                    "`{flag}` is advertised on `approvals {}` but the flat parser accepted \
                     none of {FLAG_SAMPLES:?} for it. Either its Args struct is not \
                     #[command(flatten)]ed into ApprovalsArgs, or it takes a value shape \
                     no sample covers — add one to FLAG_SAMPLES.",
                    node.name
                );
            }
        }

        assert!(
            checked >= 6,
            "the walk found only {checked} flags — a schema that advertises nothing \
             would pass this test vacuously"
        );
    }

    /// The subcommand *path* still parses with the flags, in the shape
    /// `ToolArgs::to_argv()` actually emits (flags, then `--`, then the
    /// subcommand words as positionals). The reflective guard above checks
    /// the flags alone; this checks that the `--` tail does not disturb them.
    #[test]
    fn a_flag_parses_alongside_the_subcommand_tail() {
        for argv in [
            vec!["approvals", "--all", "--", "list"],
            vec!["approvals", "--since=12", "--", "log"],
            vec!["approvals", "--until=1h", "--", "grant", "req_0badcafe_1"],
            vec!["approvals", "--reason=no", "--", "deny", "req_0badcafe_1"],
        ] {
            ApprovalsArgs::try_parse_from(argv.iter().map(|s| s.to_string()))
                .unwrap_or_else(|e| panic!("{argv:?} must parse: {e}"));
        }
    }

    #[test]
    fn humanize_prefers_the_largest_whole_unit() {
        assert_eq!(humanize(std::time::Duration::from_secs(3600)), "1h");
        assert_eq!(humanize(std::time::Duration::from_secs(300)), "5m");
        assert_eq!(humanize(std::time::Duration::from_secs(45)), "45s");
    }

    #[test]
    fn state_words_match_the_wire_spelling() {
        use kaish_types::approval::RequestState;
        assert_eq!(state_word(RequestState::Requested), "requested");
        assert_eq!(state_word(RequestState::Granted), "granted");
        assert_eq!(state_word(RequestState::Expired), "expired");
    }
}
