# kaish (会sh) Architecture

## 核 (Kaku) — Kernel-First Design

The core insight: **the 核 (kaku/kernel) is the unit of execution**, not the REPL or script.
Frontends (REPL, script runner, embedded clients) connect to kernels.

```
┌─────────────────────────────────────────────────────────────────────┐
│                          Frontends                                  │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────────┐  │
│  │    REPL     │  │   Script    │  │     Kaijutsu / Embedded     │  │
│  │             │  │   Runner    │  │                             │  │
│  └──────┬──────┘  └──────┬──────┘  └─────────────┬───────────────┘  │
└─────────┼────────────────┼───────────────────────┼──────────────────┘
          │                │                       │
          │      KernelClient (trait)              │
          │   (direct / IPC / embedded)            │
          └────────────────┴───────┬───────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    会sh 核 (Kaku) — Kernel                          │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │ State: variables, tool definitions, VFS mounts, job handles   │ │
│  └────────────────────────────────────────────────────────────────┘ │
│                           │                                         │
│                           ▼                                         │
│  ┌─────────────────────────────────────────────────────────────────┐│
│  │                    Shell Engine                                 ││
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────────┐   │
│  │    Lexer     │  │    Parser    │  │       Interpreter        │   │
│  │   (logos)    │  │   (chumsky)  │  │   (async, tokio-based)   │   │
│  └──────┬───────┘  └──────┬───────┘  └──────────┬───────────────┘   │
│         │                 │                     │                   │
│         ▼                 ▼                     ▼                   │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │                        AST Types                            │    │
│  │  Command | Pipeline | Redirect | Assignment | ToolDef | ... │    │
│  └─────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────┘
                           │
         ┌─────────────────┼─────────────────┐
         │                 │                 │
         ▼                 ▼                 ▼
┌─────────────────┐ ┌─────────────┐ ┌─────────────────┐
│   Tool Registry │ │     VFS     │ │  Job Scheduler  │
│                 │ │             │ │                 │
│ ┌─────────────┐ │ │ ┌─────────┐ │ │ ┌─────────────┐ │
│ │  Builtins   │ │ │ │ Memory  │ │ │ │ Background  │ │
│ │ echo,ls,cd..│ │ │ │ /scratch│ │ │ │   Jobs      │ │
│ └─────────────┘ │ │ └─────────┘ │ │ └─────────────┘ │
│ ┌─────────────┐ │ │ ┌─────────┐ │ │ ┌─────────────┐ │
│ │ MCP Clients │ │ │ │ LocalFs │ │ │ │  散/集      │ │
│ │ exa, fs, ...│ │ │ │  /src   │ │ │ │  Scatter/   │ │
│ └─────────────┘ │ │ └─────────┘ │ │ │   Gather    │ │
│ ┌─────────────┐ │ │ ┌─────────┐ │ │ └─────────────┘ │
│ │ User Tools  │ │ │ │MCP Res. │ │ │ ┌─────────────┐ │
│ │ (tool def)  │ │ │ │  /mcp/* │ │ │ │   Pipes     │ │
│ └─────────────┘ │ │ └─────────┘ │ │ │  (channels) │ │
└─────────────────┘ └─────────────┘ │ └─────────────┘ │
                                    └─────────────────┘
```

## Crate Structure

```
kaish/
├── Cargo.toml
├── schema/
│   └── kaish.capnp             # Cap'n Proto schema (kernel protocol)
│
├── crates/
│   ├── kaish-schema/           # Generated Cap'n Proto code
│   │   ├── Cargo.toml
│   │   ├── build.rs            # capnpc code generation
│   │   └── src/lib.rs          # Re-exports generated types
│   │
│   ├── kaish-kernel/           # Core kernel (~13,000 lines)
│   │   ├── Cargo.toml
│   │   └── src/
│   │       ├── lib.rs          # Kernel public API
│   │       ├── kernel.rs       # Kernel state & lifecycle
│   │       ├── lexer.rs        # Token definitions (logos, 1,300+ lines)
│   │       ├── parser.rs       # AST generation (chumsky, 1,100+ lines)
│   │       ├── arithmetic.rs   # Arithmetic expression evaluation (400+ lines)
│   │       ├── glob.rs         # Glob pattern engine (350+ lines)
│   │       │
│   │       ├── ast/            # AST types
│   │       │   ├── mod.rs
│   │       │   ├── types.rs    # Statement, Expression, Value types
│   │       │   └── sexpr.rs    # S-expression formatter for tests
│   │       │
│   │       ├── interpreter/    # Expression & statement evaluation (2,500+ lines)
│   │       │   ├── mod.rs
│   │       │   ├── eval.rs     # Expression evaluation
│   │       │   ├── scope.rs    # Variable scoping
│   │       │   ├── result.rs   # ExecResult type
│   │       │   └── control_flow.rs  # break/continue/return handling
│   │       │
│   │       ├── tools/          # Tool registry & 56 builtins
│   │       │   ├── mod.rs
│   │       │   ├── registry.rs # Tool lookup & dispatch
│   │       │   ├── traits.rs   # Tool trait definition
│   │       │   ├── context.rs  # Execution context
│   │       │   ├── mcp.rs      # MCP client wrapper
│   │       │   └── builtin/    # Built-in tools (echo, ls, grep, jq, etc.)
│   │       │
│   │       ├── validator/      # Pre-execution validation (1,200+ lines)
│   │       │   ├── mod.rs
│   │       │   └── walker.rs   # AST walker for validation
│   │       │
│   │       ├── vfs/            # Virtual filesystem (2,200+ lines)
│   │       │   ├── mod.rs
│   │       │   ├── traits.rs   # Filesystem trait
│   │       │   ├── memory.rs   # In-memory (/scratch)
│   │       │   ├── local.rs    # Local filesystem
│   │       │   ├── git.rs      # Git repository introspection
│   │       │   └── router.rs   # Mount point routing
│   │       │
│   │       ├── scheduler/      # Job scheduling (1,200+ lines)
│   │       │   ├── mod.rs
│   │       │   ├── pipeline.rs # Pipeline execution
│   │       │   ├── jobs.rs     # Background job management
│   │       │   └── scatter_gather.rs  # Parallel fan-out/collection
│   │       │
│   │       ├── backend/        # File operations abstraction (1,600+ lines)
│   │       │   ├── mod.rs
│   │       │   └── local.rs    # Local filesystem operations
│   │       │
│   │       ├── walker/         # File traversal & globbing (1,000+ lines)
│   │       │   ├── mod.rs
│   │       │   ├── glob_path.rs
│   │       │   ├── filter.rs
│   │       │   └── ignore.rs   # .gitignore support
│   │       │
│   │       ├── paths.rs        # XDG path helpers
│   │       │
│   │       └── mcp/            # MCP client integration (400+ lines)
│   │           ├── mod.rs
│   │           └── client.rs   # External MCP server connection
│   │
│   ├── kaish-client/           # Client trait + implementations (~585 lines)
│   │   ├── Cargo.toml
│   │   └── src/
│   │       ├── lib.rs
│   │       ├── traits.rs       # KernelClient trait
│   │       ├── embedded.rs     # Direct in-process kernel
│   │       └── ipc.rs          # Unix socket + Cap'n Proto RPC
│   │
│   └── kaish-repl/             # Interactive REPL frontend (~1,900 lines)
│       ├── Cargo.toml
│       └── src/
│           ├── lib.rs          # Main REPL loop (rustyline)
│           ├── main.rs         # CLI entry point
│           └── format.rs       # Output formatting & hints
│
└── tests/                      # (in crates/kaish-kernel/tests/)
    ├── lexer_tests.rs          # 94 parameterized lexer tests
    ├── parser_tests.rs         # 101 snapshot tests for AST
    ├── validation_tests.rs     # Pre-execution validation tests
    ├── realworld_builtin_tests.rs  # Integration tests
    └── snapshots/              # Insta snapshot files
```

**Note:** `kaish-mcp` (MCP server frontend to export kaish tools as MCP) is planned but not yet implemented. The kernel includes MCP *client* integration for consuming external MCP tools.

## 核 Architecture

### Kernel State

```rust
pub struct Kernel {
    /// Variable bindings (scoped)
    variables: Scope,

    /// User-defined tools from `tool` statements
    user_tools: HashMap<String, ToolDef>,

    /// Registered MCP servers
    mcp_clients: HashMap<String, Box<dyn McpClient>>,

    /// Virtual filesystem with mount points
    vfs: VfsRouter,

    /// Background jobs
    jobs: JobManager,

    /// Last command result ($?)
    last_result: ExecResult,
}
```

### 核 Protocol (Cap'n Proto)

The kernel protocol is defined in `schema/kaish.capnp`. Cap'n Proto gives us:
- **Zero-copy reads** - no deserialization overhead for IPC
- **Streaming RPC** - built-in support for output streaming
- **Schema evolution** - add fields without breaking clients
- **Capability-based security** - natural fit for tool permissions

```capnp
interface Kernel {
  # Execution
  execute @0 (input :Text) -> (result :ExecResult);
  executeStreaming @1 (input :Text) -> (stream :OutputStream);

  # Variables
  getVar @2 (name :Text) -> (value :Value);
  setVar @3 (name :Text, value :Value) -> ();
  listVars @4 () -> (vars :List(KeyValue));

  # Tools
  listTools @5 () -> (tools :List(ToolInfo));
  callTool @7 (name :Text, args :List(KeyValue)) -> (result :ExecResult);

  # Jobs
  listJobs @8 () -> (jobs :List(JobInfo));
  cancelJob @9 (id :UInt64) -> (success :Bool);

  # State persistence
  snapshot @17 () -> (state :KernelState);
  restore @18 (state :KernelState) -> ();

  # Lifecycle
  ping @20 () -> (pong :Text);
  shutdown @21 () -> ();
}
```

### KernelClient Implementations

```rust
// Generated from kaish.capnp
use kaish_schema::kernel_capnp::kernel;

// === Implementations ===

/// Direct in-process kernel (Kaijutsu uses this)
/// Bypasses serialization entirely for performance
pub struct EmbeddedClient {
    kernel: Arc<RwLock<Kernel>>,
}

/// Connect to kernel over Unix socket via Cap'n Proto RPC
pub struct IpcClient {
    client: kernel::Client,
    connection: RpcConnection,
}
```

### 核 Lifecycle

```
┌─────────────────────────────────────────────────────────────────────┐
│                      Kernel Lifecycle                               │
│                                                                     │
│  ┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐          │
│  │  New    │───▶│  Init   │───▶│ Running │───▶│ Shutdown│          │
│  │         │    │         │    │         │    │         │          │
│  └─────────┘    └─────────┘    └─────────┘    └─────────┘          │
│                      │              │                               │
│                      │              │                               │
│                      ▼              ▼                               │
│              Load config      Execute statements                   │
│              Mount VFS        Run background jobs                  │
│              Register MCP     Handle tool calls                    │
│              Load tools.kai   Scatter/gather                       │
└─────────────────────────────────────────────────────────────────────┘
```

### Usage Patterns

```rust
// === Embedded (Kaijutsu) ===
let kernel = Kernel::new();
kernel.mount("/workspace", local_fs);
kernel.register_mcp("exa", exa_client);

let client = EmbeddedClient::new(kernel);
let result = client.execute("ls /workspace").await?;

// === REPL with kernel ===
let client = IpcClient::connect("/tmp/kaish.sock")?;
// Kernel process manages its own lifecycle

// === MCP Server ===
let kernel = Kernel::new();
kernel.load_file("tools.kai").await?;  // define tools
McpServer::new(kernel).serve_stdio().await?;

// === Script Runner ===
let kernel = Kernel::new();
let client = EmbeddedClient::new(kernel);
let script = fs::read_to_string("script.kai")?;
client.execute(&script).await?;
```

## 核 Discovery (Socket Files)

Kernels are discovered via socket files:

```
/tmp/kaish-$USER/
├── default.sock             # Default kernel
├── default.pid              # PID for stale detection
├── project-foo.sock         # Named kernel
└── session-abc123.sock      # Ephemeral session
```

### Socket File Protocol

```rust
// Kernel creates socket on startup
let socket_path = format!("/tmp/kaish-{}/{}.sock", user, kernel_name);
let listener = UnixListener::bind(&socket_path)?;

// Write pid file for cleanup detection
fs::write(format!("{}.pid", socket_path), std::process::id().to_string())?;

// Client connects
let stream = UnixStream::connect(&socket_path)?;
let client = capnp_rpc::new_client(stream);
```

### Stale Socket Cleanup

```rust
fn cleanup_stale_sockets(dir: &Path) -> io::Result<()> {
    for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        if path.extension() == Some("sock") {
            let pid_file = path.with_extension("pid");
            if let Ok(pid_str) = fs::read_to_string(&pid_file) {
                let pid: u32 = pid_str.trim().parse().unwrap_or(0);
                if !process_exists(pid) {
                    fs::remove_file(&path)?;
                    fs::remove_file(&pid_file)?;
                }
            }
        }
    }
    Ok(())
}
```

### REPL 核 Connection

```rust
// REPL startup logic
fn connect_to_kernel(name: Option<&str>) -> Result<KernelClient> {
    let socket_dir = format!("/tmp/kaish-{}/", whoami::username());
    cleanup_stale_sockets(&socket_dir)?;

    let socket_name = name.unwrap_or("default");
    let socket_path = format!("{}{}.sock", socket_dir, socket_name);

    if Path::new(&socket_path).exists() {
        // Connect to existing kernel
        IpcClient::connect(&socket_path)
    } else {
        // Start new kernel
        let kernel = Kernel::new(&format!("{}{}.db", data_dir(), socket_name))?;
        let listener = kernel.listen(&socket_path)?;

        // Fork to background or return embedded client
        EmbeddedClient::new(kernel)
    }
}
```

## Key Types

```rust
// === AST ===

pub enum Stmt {
    Command(Command),
    Pipeline(Vec<PipelineStage>),
    Assignment(Assignment),
    If(IfStmt),
    For(ForLoop),
    ToolDef(ToolDef),
    Background(Box<Stmt>),
}

pub struct Command {
    pub name: String,
    pub args: Vec<Arg>,
    pub redirects: Vec<Redirect>,
}

pub enum Arg {
    Positional(Value),
    Named { key: String, value: Value },
}

pub enum Value {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Null,
    VarRef(String),                // $VAR or ${VAR}
    Interpolated(Vec<StringPart>), // "hello ${name}"
}

pub struct ToolDef {
    pub name: String,
    pub params: Vec<ParamDef>,
    pub body: Vec<Stmt>,
}

pub struct ParamDef {
    pub name: String,
    pub typ: ParamType,
    pub default: Option<Value>,
}

// === Runtime ===

pub struct ExecResult {
    pub code: i32,
    pub ok: bool,
    pub err: Option<String>,
    pub out: String,
    pub data: Option<serde_json::Value>,
}

#[async_trait]
pub trait Tool: Send + Sync {
    fn name(&self) -> &str;
    fn schema(&self) -> ToolSchema;
    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult;
}

// === VFS ===

#[async_trait]
pub trait Filesystem: Send + Sync {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>>;
    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()>;
    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>>;
    async fn stat(&self, path: &Path) -> io::Result<Metadata>;
    async fn mkdir(&self, path: &Path) -> io::Result<()>;
    async fn remove(&self, path: &Path) -> io::Result<()>;
}

pub struct VfsRouter {
    mounts: BTreeMap<PathBuf, Box<dyn Filesystem>>,
}
```

## Shebang Mode

Trivial to implement:

```rust
fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.get(1).map(|s| s.as_str()) {
        None => run_repl(),
        Some("serve") => run_server(&args[2..]),
        Some(path) => run_script(path),
    }
}

fn run_script(path: &str) -> Result<()> {
    let source = std::fs::read_to_string(path)?;

    // Skip shebang if present
    let source = if source.starts_with("#!") {
        source.lines().skip(1).collect::<Vec<_>>().join("\n")
    } else {
        source
    };

    let mut shell = Kaish::new();
    shell.execute(&source)
}
```

The shebang (`#!/usr/bin/env kaish`) is handled by the OS — it invokes our binary with the script path. We skip line 1 if it starts with `#!`.

## MCP Server Mode: The Prestige (Planned)

> **Status:** This feature is planned but not yet implemented. The `kaish-mcp` crate does not exist yet.

The vision: `kaish serve tools.kai` would:

1. Parse the script, extract all `tool` definitions
2. Build MCP tool schemas from the `ParamDef`s
3. Start MCP server (stdio or HTTP)
4. On tool call: instantiate a shell, execute the tool body

```rust
// Planned implementation
async fn serve_script(path: &str) -> Result<()> {
    let source = std::fs::read_to_string(path)?;
    let ast = parse(&source)?;

    // Extract tool definitions
    let tools: Vec<ToolDef> = ast.iter()
        .filter_map(|s| match s {
            Stmt::ToolDef(t) => Some(t.clone()),
            _ => None,
        })
        .collect();

    // Build MCP server
    let server = McpServer::new();
    for tool in tools {
        server.register(ScriptTool::new(tool, source.clone()));
    }

    // Run server
    server.serve_stdio().await
}
```

This would allow Claude Code to call tools defined in kaish scripts.
**User-defined tools would become first-class MCP tools.**

## Parser: logos + chumsky

- **logos** for lexer: fast, derive-macro based
- **chumsky** for parser: beautiful errors, declarative grammar

```rust
// Lexer with logos
#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("set")]
    Set,
    #[token("tool")]
    Tool,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    // ...
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_-]*")]
    Ident,
    #[regex(r"-?[0-9]+")]
    Int,
    // ...
}

// Parser with chumsky
fn command() -> impl Parser<Token, Command, Error = Simple<Token>> {
    ident()
        .then(arg().repeated())
        .then(redirect().repeated())
        .map(|((name, args), redirects)| Command { name, args, redirects })
}
```

## Async Model

Everything is async (tokio):
- Tool execution is async (MCP calls, file I/O)
- Pipelines spawn tasks for each stage
- Scatter/gather uses `JoinSet` for parallel execution
- Background jobs are spawned tasks with handles

```rust
// Pipeline execution
async fn execute_pipeline(stages: Vec<PipelineStage>, ctx: &mut ExecContext) -> ExecResult {
    let (mut tx, mut rx) = mpsc::channel::<Value>(32);

    let mut handles = Vec::new();
    for (i, stage) in stages.iter().enumerate() {
        let (next_tx, next_rx) = if i < stages.len() - 1 {
            let (tx, rx) = mpsc::channel(32);
            (Some(tx), Some(rx))
        } else {
            (None, None)
        };

        let handle = tokio::spawn(execute_stage(stage, rx, next_tx));
        handles.push(handle);
        rx = next_rx.unwrap_or_else(|| mpsc::channel(1).1);
    }

    // Wait for completion, collect final result
    // ...
}
```

## Integration with Kaijutsu

**kaish is the execution engine. Kaijutsu wraps it with collaboration.**

### Interface Ownership

| Interface | Owner | Purpose |
|-----------|-------|---------|
| `kaish.capnp::Kernel` | **kaish** | Execution: parse, eval, tools, VFS, MCP, state, blobs |
| `kaijutsu.capnp::World` | **kaijutsu** | Multi-kernel orchestration |
| `kaijutsu.capnp::Kernel` | **kaijutsu** | Collaboration: lease, consent, fork/thread, checkpoint, messaging |

### Embedding Pattern

kaijutsu-server embeds kaish-kernel directly (no IPC overhead):

```rust
// In kaijutsu-server
use kaish_kernel::{Kernel, EmbeddedClient};

// Create kaish kernel for this session
let kaish = Kernel::builder()
    .mount("/mnt/project", worktree_vfs)
    .mount("/scratch", MemoryFs::new())
    .register_mcp("exa", exa_client)
    .build();

let client = EmbeddedClient::new(kaish);

// When kaijutsu.capnp::Kernel.execute() is called:
let result = client.execute("ls /mnt/project | grep pattern=rs").await?;

// kaijutsu adds collaboration on top:
// - Acquire lease before execute
// - Record in message DAG
// - Release lease after
// - Checkpoint if autonomous mode triggers it
```

### Standalone Mode

kaish also runs independently (without kaijutsu):

```bash
# Interactive REPL
kaish

# Run script
kaish script.kai

# RPC server (other tools can connect)
kaish serve --socket=/tmp/kaish.sock

# MCP server (Claude Code can call kaish tools)
kaish serve tools.kai --stdio
```

### Context Generation

kaish provides `context-emit` for generating AI context payloads:

```bash
# Generate Claude-format context from kernel state
kaish context-emit --format=claude

# Include specific mounts, since last checkpoint
kaish context-emit --format=openai --include=/mnt/project --since=checkpoint:latest
```

This is the mechanism by which AI "attaches" to a kernel — context is generated fresh from kernel state + VFS, not stored.

