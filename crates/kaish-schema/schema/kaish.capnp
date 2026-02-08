@0xb78d9e8a7c6f5e4d;  # Unique file ID

# kaish (会sh) Cap'n Proto Schema
# Kernel protocol + state serialization
#
# This schema defines the wire protocol for communicating with a kaish kernel.
# Sections are organized in dependency order: foundational types first,
# then higher-level types that reference them.

# ============================================================
# 1. Foundational Types
# ============================================================
# Core value representation used throughout the protocol.

struct KeyValue {
  key @0 :Text;
  value @1 :Value;
}

struct Value {
  union {
    null @0 :Void;
    bool @1 :Bool;
    int @2 :Int64;
    float @3 :Float64;
    string @4 :Text;
    array @5 :List(Value);
    object @6 :List(KeyValue);
    blob @7 :BlobRef;              # Large binary data (streamed)
  }
}

# Reference to a blob stored elsewhere (VFS, SQLite, etc.)
struct BlobRef {
  id @0 :Text;                     # Unique identifier
  size @1 :UInt64;                 # Size in bytes
  contentType @2 :Text;            # MIME type hint
  hash @3 :Data;                   # SHA-256 for integrity (optional)
}

# ============================================================
# 2. Streaming Types
# ============================================================
# Interfaces for streaming large data and command output.

# For streaming large blobs over RPC
interface BlobStream {
  # Read next chunk (returns empty when done)
  read @0 (maxBytes :UInt32) -> (data :Data, done :Bool);

  # Get total size if known
  size @1 () -> (bytes :UInt64, known :Bool);

  # Cancel streaming
  cancel @2 () -> ();
}

# For writing blobs
interface BlobSink {
  write @0 (data :Data) -> ();
  finish @1 () -> (hash :Data);  # Returns SHA-256
  abort @2 () -> ();
}

# Output streaming for long-running commands
interface OutputStream {
  read @0 () -> (chunk :OutputChunk);
  cancel @1 () -> ();
}

struct OutputChunk {
  union {
    stdout @0 :Data;
    stderr @1 :Data;
    done @2 :ExecResult;
    error @3 :Text;
  }
}

# ============================================================
# 3. Tool Metadata
# ============================================================
# Information about available tools (builtins, user-defined, MCP).

struct ToolInfo {
  name @0 :Text;
  description @1 :Text;
  source @2 :ToolSource;
}

enum ToolSource {
  builtin @0;
  user @1;
  mcp @2;
}

# ============================================================
# 4. Structured Output (Tree-of-Tables Model)
# ============================================================
# Unified structured output model where builtins return data
# and frontends handle all rendering.

# Entry type for rendering hints (colors, icons).
enum EntryType {
  text @0;        # Generic text content
  file @1;        # Regular file
  directory @2;   # Directory
  executable @3;  # Executable file
  symlink @4;     # Symbolic link
}

# A node in the output tree.
# Nodes can carry text, tabular cells, and nested children.
struct OutputNode {
  name @0 :Text;                  # Primary identifier (filename, key, label)
  entryType @1 :EntryType;        # Rendering hint (colors, icons)
  text @2 :Text;                  # Text content (for echo, cat, exec)
  cells @3 :List(Text);           # Additional columns (for ls -l, ps, env)
  children @4 :List(OutputNode);  # Child nodes (for tree, find)
}

# Structured output data from a command.
struct OutputData {
  headers @0 :List(Text);         # Column headers (optional)
  root @1 :List(OutputNode);      # Top-level nodes
}

# ============================================================
# 5. Execution Results
# ============================================================
# Structured result from command execution.

struct ExecResult {
  code @0 :Int32;
  ok @1 :Bool;
  err @2 :Text;       # Empty if ok
  stdout @3 :Data;    # Raw bytes
  stderr @4 :Data;    # Raw bytes
  data @5 :Value;     # Parsed JSON if applicable
  reserved6 @6 :Void; # Was hint, removed in structured output migration
  output @7 :OutputData; # Structured output (unified model)
}

# ============================================================
# 6. VFS Configuration
# ============================================================
# Virtual filesystem mount configuration.

struct MountConfig {
  path @0 :Text;           # Mount point, e.g., "/src"
  backend @1 :MountBackend;
  readOnly @2 :Bool;       # Reject write operations
}

struct MountBackend {
  union {
    memory @0 :Void;                    # In-memory scratch
    local @1 :LocalFsConfig;            # Local filesystem
    mcp @2 :McpResourceConfig;          # MCP server resources
  }
}

struct LocalFsConfig {
  rootPath @0 :Text;       # Actual filesystem path
}

struct McpResourceConfig {
  serverName @0 :Text;     # MCP server name
  resourcePrefix @1 :Text; # Resource URI prefix
}

# ============================================================
# 7. MCP Server Configuration
# ============================================================
# Configuration for connecting to external MCP servers.

struct McpServerConfig {
  name @0 :Text;           # Local name, e.g., "exa"
  transport @1 :McpTransport;
}

struct McpTransport {
  union {
    stdio @0 :StdioTransport;
    http @1 :HttpTransport;
    sse @2 :SseTransport;
  }
}

struct StdioTransport {
  command @0 :Text;
  args @1 :List(Text);
  env @2 :List(KeyValue);
}

struct HttpTransport {
  baseUrl @0 :Text;
  headers @1 :List(KeyValue);
}

struct SseTransport {
  url @0 :Text;
  headers @1 :List(KeyValue);
}

# ============================================================
# 8. State Management
# ============================================================
# Kernel state for serialization (snapshot/restore).
#
# Note: snapshot and restore are defined in the Kernel interface but
# currently return "unimplemented" errors. When implemented, additional
# types (ToolDef, ParamDef, ParamType) will be added for user-defined tools.

struct KernelState {
  version @0 :UInt32;              # Schema version for migration
  timestamp @1 :Int64;             # Unix timestamp of snapshot

  variables @2 :List(KeyValue);    # Variable bindings
  toolsReserved @3 :Void;          # Reserved - was List(ToolDef), add back for snapshot/restore
  mounts @4 :List(MountConfig);    # VFS configuration
  mcpServers @5 :List(McpServerConfig);  # MCP connections

  lastResult @6 :ExecResult;       # $? state
  cwd @7 :Text;                    # Current working directory

  # Optional metadata
  sessionId @8 :Text;
  metadata @9 :List(KeyValue);     # Extensible
}

# ============================================================
# 9. Kernel Protocol (RPC Interface)
# ============================================================
# The main interface for communicating with a kaish kernel.
#
# Ordinal History:
# - @0-@20: Initial release
# - @21-@23: Added getCwd, setCwd, getLastResult
#
# Implementation Status:
# - Fully implemented: execute, get/set/listVars, getCwd, setCwd,
#   getLastResult, listTools, read/write/deleteBlob, ping, shutdown, reset
# - Unimplemented (return errors): executeStreaming, callTool, mount,
#   unmount, listMounts, register/unregister/listMcpServers, snapshot, restore

interface Kernel {
  # --- Execution ---
  execute @0 (input :Text) -> (result :ExecResult);
  executeStreaming @1 (input :Text) -> (stream :OutputStream);

  # --- Variables ---
  getVar @2 (name :Text) -> (value :Value);
  setVar @3 (name :Text, value :Value) -> ();
  listVars @4 () -> (vars :List(KeyValue));

  # --- Tools ---
  listTools @5 () -> (tools :List(ToolInfo));
  callTool @6 (name :Text, args :List(KeyValue)) -> (result :ExecResult);

  # --- VFS ---
  mount @7 (config :MountConfig) -> ();
  unmount @8 (path :Text) -> ();
  listMounts @9 () -> (mounts :List(MountConfig));

  # --- MCP ---
  registerMcp @10 (config :McpServerConfig) -> ();
  unregisterMcp @11 (name :Text) -> ();
  listMcpServers @12 () -> (servers :List(McpServerConfig));

  # --- State ---
  snapshot @13 () -> (state :KernelState);
  restore @14 (state :KernelState) -> ();
  reset @15 () -> ();  # Clear all state

  # --- Blobs ---
  readBlob @16 (id :Text) -> (stream :BlobStream);
  writeBlob @17 (contentType :Text, size :UInt64) -> (id :Text, stream :BlobSink);
  deleteBlob @18 (id :Text) -> (success :Bool);

  # --- Lifecycle ---
  ping @19 () -> (pong :Text);
  shutdown @20 () -> ();

  # --- Working Directory (added after initial release) ---
  getCwd @21 () -> (path :Text);
  setCwd @22 (path :Text) -> (success :Bool, error :Text);

  # --- Last Result (added after initial release) ---
  getLastResult @23 () -> (result :ExecResult);
}
