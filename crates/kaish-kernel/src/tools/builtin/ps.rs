//! ps — List processes.
//!
//! A simplified process listing tool that outputs JSON instead of `-o` formatting.
//! Covers the common 80% use case while being pipeline-friendly with `--json`.

use async_trait::async_trait;
use std::collections::HashMap;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Process information for output.
#[derive(Debug, Clone)]
struct ProcessInfo {
    pid: i32,
    ppid: i32,
    user: String,
    state: String,
    cpu: f64,
    mem: f64,
    vsz: u64,
    rss: u64,
    tty: Option<String>,
    time: String,
    command: String,
}

/// Ps tool: list processes.
pub struct Ps;

#[async_trait]
impl Tool for Ps {
    fn name(&self) -> &str {
        "ps"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("ps", "List processes")
            .param(
                ParamSchema::optional("all", "bool", Value::Bool(false), "Show all processes, not just current user's (-a)")
                    .with_aliases(["-a", "--all"]),
            )
            .param(
                ParamSchema::optional("pid", "int", Value::Null, "Filter to specific PID (-p)")
                    .with_aliases(["-p", "--pid"]),
            )
            .param(
                ParamSchema::optional("user", "string", Value::Null, "Filter by username (-u)")
                    .with_aliases(["-u", "--user"]),
            )
            .example("Current user's processes", "ps")
            .example("All processes", "ps -a")
            .example("Specific PID", "ps pid=1234 -a")
            .example("Filter by user", "ps user=root -a")
            .example("JSON output", "ps --json")
            .example("All processes as JSON", "ps -a --json")
            .example("JSON with jq", "ps -a --json | jq '.[].command'")
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        let show_all = args.has_flag("all") || args.has_flag("a");

        let filter_pid = args
            .get("pid", usize::MAX)
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as i32),
                Value::String(s) => s.parse().ok(),
                _ => None,
            });

        let filter_user = args.get_string("user", usize::MAX);

        // Get current UID for filtering (from /proc/self/status)
        let current_uid = get_current_uid();

        // Build user cache for UID -> username lookup
        let user_cache = build_user_cache();

        // Get system memory and page size for percentage calculation
        let total_memory_kb = get_total_memory_kb();
        let page_size = procfs::page_size();

        // Get system uptime for CPU % calculation
        let uptime_seconds = get_uptime_seconds();

        // Collect process information
        let mut processes = Vec::new();

        let all_procs = match procfs::process::all_processes() {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("ps: failed to read /proc: {}", e)),
        };

        let ticks_per_sec = procfs::ticks_per_second();

        for proc_result in all_procs {
            let proc = match proc_result {
                Ok(p) => p,
                Err(_) => continue, // Process may have exited
            };

            let stat = match proc.stat() {
                Ok(s) => s,
                Err(_) => continue,
            };

            let status = proc.status().ok();
            let cmdline = proc.cmdline().ok();

            // Get owner UID from status
            let owner_uid = status
                .as_ref()
                .map(|s| s.ruid)
                .unwrap_or(0);

            // Apply filters
            if !show_all && owner_uid != current_uid {
                continue;
            }

            if let Some(pid) = filter_pid
                && stat.pid != pid {
                    continue;
                }

            let username = user_cache
                .get(&owner_uid)
                .cloned()
                .unwrap_or_else(|| owner_uid.to_string());

            if let Some(ref user) = filter_user
                && &username != user {
                    continue;
                }

            // Calculate RSS in KB
            let rss_kb = (stat.rss * page_size) / 1024;

            // Calculate memory percentage
            let mem_percent = if total_memory_kb > 0 {
                (rss_kb as f64 / total_memory_kb as f64) * 100.0
            } else {
                0.0
            };

            // Calculate CPU percentage
            let cpu_percent = calculate_cpu_percent(&stat, uptime_seconds, ticks_per_sec);

            // Get TTY
            let tty = get_tty_name(stat.tty_nr);

            // Format CPU time
            let total_ticks = stat.utime + stat.stime;
            let total_seconds = total_ticks / ticks_per_sec;
            let minutes = total_seconds / 60;
            let seconds = total_seconds % 60;
            let time = format!("{}:{:02}", minutes, seconds);

            // Get command
            let command = cmdline
                .as_ref()
                .filter(|c| !c.is_empty())
                .map(|c| c.join(" "))
                .unwrap_or_else(|| format!("[{}]", stat.comm));

            processes.push(ProcessInfo {
                pid: stat.pid,
                ppid: stat.ppid,
                user: username,
                state: stat.state.to_string(),
                cpu: round_to_1(cpu_percent),
                mem: round_to_1(mem_percent),
                vsz: stat.vsize / 1024, // Convert to KB
                rss: rss_kb,
                tty,
                time,
                command,
            });
        }

        // Sort by PID
        processes.sort_by_key(|p| p.pid);

        format_table(&processes)
    }
}

/// Get current user's UID by reading /proc/self/status.
fn get_current_uid() -> u32 {
    procfs::process::Process::myself()
        .ok()
        .and_then(|p| p.status().ok())
        .map(|s| s.ruid)
        .unwrap_or(0)
}

/// Build a cache of UID -> username mappings.
fn build_user_cache() -> HashMap<u32, String> {
    let mut cache = HashMap::new();

    // Read /etc/passwd to build the cache
    if let Ok(content) = std::fs::read_to_string("/etc/passwd") {
        for line in content.lines() {
            let parts: Vec<&str> = line.split(':').collect();
            if parts.len() >= 3
                && let Ok(uid) = parts[2].parse::<u32>() {
                    cache.insert(uid, parts[0].to_string());
                }
        }
    }

    cache
}

/// Get total system memory in KB by reading /proc/meminfo.
fn get_total_memory_kb() -> u64 {
    if let Ok(content) = std::fs::read_to_string("/proc/meminfo") {
        for line in content.lines() {
            if line.starts_with("MemTotal:") {
                // Format: "MemTotal:       16384000 kB"
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 {
                    return parts[1].parse().unwrap_or(0);
                }
            }
        }
    }
    0
}

/// Get system uptime in seconds by reading /proc/uptime.
fn get_uptime_seconds() -> f64 {
    if let Ok(content) = std::fs::read_to_string("/proc/uptime") {
        // Format: "12345.67 23456.78" (uptime idle_time)
        if let Some(uptime_str) = content.split_whitespace().next() {
            return uptime_str.parse().unwrap_or(1.0);
        }
    }
    1.0
}

/// Calculate CPU percentage based on process time and system uptime.
fn calculate_cpu_percent(stat: &procfs::process::Stat, uptime_seconds: f64, ticks_per_sec: u64) -> f64 {
    let ticks_per_sec_f64 = ticks_per_sec as f64;
    let process_ticks = (stat.utime + stat.stime) as f64;
    let starttime_sec = stat.starttime as f64 / ticks_per_sec_f64;
    let elapsed = uptime_seconds - starttime_sec;

    if elapsed > 0.0 {
        (process_ticks / ticks_per_sec_f64 / elapsed) * 100.0
    } else {
        0.0
    }
}

/// Convert TTY number to name.
fn get_tty_name(tty_nr: i32) -> Option<String> {
    if tty_nr == 0 {
        return None;
    }

    let major = (tty_nr >> 8) & 0xff;
    let minor = tty_nr & 0xff;

    match major {
        4 => Some(format!("tty{}", minor)), // Virtual console
        136..=143 => Some(format!("pts/{}", minor)), // PTY slaves
        _ => Some(format!("?/{}", tty_nr)),
    }
}

/// Round to 1 decimal place.
fn round_to_1(val: f64) -> f64 {
    (val * 10.0).round() / 10.0
}

/// Format processes as structured OutputData table.
fn format_table(processes: &[ProcessInfo]) -> ExecResult {
    if processes.is_empty() {
        return ExecResult::with_output(OutputData::new());
    }

    let headers = vec![
        "PID".to_string(),
        "PPID".to_string(),
        "USER".to_string(),
        "S".to_string(),
        "%CPU".to_string(),
        "%MEM".to_string(),
        "VSZ".to_string(),
        "RSS".to_string(),
        "TTY".to_string(),
        "TIME".to_string(),
        "COMMAND".to_string(),
    ];

    let nodes: Vec<OutputNode> = processes
        .iter()
        .map(|p| {
            let tty = p.tty.as_deref().unwrap_or("?");
            OutputNode::new(p.pid.to_string()).with_cells(vec![
                p.ppid.to_string(),
                p.user.clone(),
                p.state.clone(),
                format!("{:.1}", p.cpu),
                format!("{:.1}", p.mem),
                p.vsz.to_string(),
                p.rss.to_string(),
                tty.to_string(),
                p.time.clone(),
                p.command.clone(),
            ])
        })
        .collect();

    ExecResult::with_output(OutputData::table(headers, nodes))
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_ps_basic() {
        let mut ctx = make_ctx();
        let result = Ps.execute(ToolArgs::new(), &mut ctx).await;
        // Should succeed and show at least the current process
        assert!(result.ok());
        // Check OutputData is present with table headers
        assert!(result.output.is_some());
        let output = result.output.as_ref().unwrap();
        assert!(output.headers.is_some());
        let headers = output.headers.as_ref().unwrap();
        assert!(headers.contains(&"PID".to_string()));
        assert!(headers.contains(&"COMMAND".to_string()));
    }

    #[tokio::test]
    async fn test_ps_all() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("a".to_string());

        let result = Ps.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should have OutputData with processes
        assert!(result.output.is_some());
        let output = result.output.as_ref().unwrap();
        assert!(!output.root.is_empty());
    }

    #[tokio::test]
    async fn test_ps_json_via_global_flag() {
        use crate::interpreter::{apply_output_format, OutputFormat};

        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Ps.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Simulate global --json (handled by kernel)
        let result = apply_output_format(result, OutputFormat::Json);

        // Should be valid JSON array of objects with table headers as keys
        let parsed: serde_json::Value = serde_json::from_str(&result.out).expect("valid JSON");
        let arr = parsed.as_array().expect("should be an array");
        assert!(!arr.is_empty());
        // Each entry should have PID and COMMAND keys (table headers)
        let first = arr[0].as_object().expect("should be an object");
        assert!(first.contains_key("PID"));
        assert!(first.contains_key("COMMAND"));
    }

    #[tokio::test]
    async fn test_ps_pid_filter() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named.insert("pid".to_string(), Value::Int(1));
        args.flags.insert("a".to_string()); // Need -a to see pid 1

        let result = Ps.execute(args, &mut ctx).await;
        assert!(result.ok());
        // May or may not find PID 1 depending on permissions
    }

    #[tokio::test]
    async fn test_ps_json_all_via_global_flag() {
        use crate::interpreter::{apply_output_format, OutputFormat};

        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("a".to_string());

        let result = Ps.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Simulate global --json (handled by kernel)
        let result = apply_output_format(result, OutputFormat::Json);

        let parsed: serde_json::Value = serde_json::from_str(&result.out).expect("valid JSON");
        let procs = parsed.as_array().expect("should be an array");
        // Should have multiple processes
        assert!(!procs.is_empty());

        // Each process should have all table headers as keys
        for p in procs {
            let obj = p.as_object().expect("each entry should be an object");
            assert!(obj.contains_key("PID"));
            assert!(obj.contains_key("USER"));
            assert!(obj.contains_key("COMMAND"));
            // PID should be a non-empty string (all values are strings from OutputData)
            let pid = obj["PID"].as_str().expect("PID should be a string");
            assert!(!pid.is_empty());
            let cmd = obj["COMMAND"].as_str().expect("COMMAND should be a string");
            assert!(!cmd.is_empty());
        }
    }

    #[test]
    fn test_round_to_1() {
        assert_eq!(round_to_1(3.14159), 3.1);
        assert_eq!(round_to_1(2.95), 3.0);
        assert_eq!(round_to_1(0.04), 0.0);
    }

    #[test]
    fn test_get_tty_name() {
        assert_eq!(get_tty_name(0), None);
        assert_eq!(get_tty_name(0x8800), Some("pts/0".to_string())); // 136 << 8 + 0
        assert_eq!(get_tty_name(0x401), Some("tty1".to_string())); // 4 << 8 + 1
    }
}
