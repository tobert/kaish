//! Cap'n Proto RPC server for the Kernel (核).
//!
//! This module implements the Kernel interface defined in `schema/kaish.capnp`,
//! allowing remote clients to connect to a kernel via Unix sockets.
//!
//! # Architecture
//!
//! ```text
//! Client                    Server
//!   │                         │
//!   │─── Unix Socket ────────▶│
//!   │                         │
//!   │    Cap'n Proto RPC      │
//!   │◀───────────────────────▶│
//!   │                         │
//!   │         Kernel          │
//!   │    (execute, vars, etc) │
//! ```

use std::path::Path;
use std::sync::Arc;

use anyhow::{Context, Result};
use capnp_rpc::{rpc_twoparty_capnp, twoparty, RpcSystem};
use futures::AsyncReadExt;
use tokio::net::UnixListener;
use tokio_util::compat::TokioAsyncReadCompatExt;

use kaish_schema::kernel;

use crate::kernel::Kernel;
use crate::paths as state_paths;

/// RPC server wrapper around a Kernel.
///
/// Implements the Cap'n Proto Kernel interface to serve remote requests.
pub struct KernelRpcServer {
    kernel: Arc<Kernel>,
}

impl KernelRpcServer {
    /// Create a new RPC server wrapping the given kernel.
    pub fn new(kernel: Kernel) -> Self {
        Self {
            kernel: Arc::new(kernel),
        }
    }

    /// Serve RPC requests on the default socket path.
    ///
    /// Socket is created at `$XDG_RUNTIME_DIR/kaish/<kernel_name>.sock`
    pub async fn serve_default(&self) -> Result<()> {
        let socket_path = state_paths::runtime_dir().join(format!("{}.sock", self.kernel.name()));
        self.serve(&socket_path).await
    }

    /// Serve RPC requests on the given Unix socket path.
    pub async fn serve(&self, socket_path: &Path) -> Result<()> {
        // Ensure parent directory exists
        if let Some(parent) = socket_path.parent() {
            std::fs::create_dir_all(parent).ok();
        }

        // Remove existing socket if present
        if socket_path.exists() {
            std::fs::remove_file(socket_path)
                .with_context(|| format!("removing old socket: {}", socket_path.display()))?;
        }

        let listener = UnixListener::bind(socket_path)
            .with_context(|| format!("binding to socket: {}", socket_path.display()))?;

        tracing::info!("Kernel RPC server listening on {}", socket_path.display());

        loop {
            let (stream, _addr) = listener.accept().await?;

            // Convert tokio stream to futures-compatible stream
            let stream = stream.compat();

            let (reader, writer) = stream.split();

            // Create the RPC network
            let network = twoparty::VatNetwork::new(
                reader,
                writer,
                rpc_twoparty_capnp::Side::Server,
                Default::default(),
            );

            // Create the kernel server implementation
            let kernel_impl = KernelImpl::new(self.kernel.clone());
            let kernel_client: kernel::Client = capnp_rpc::new_client(kernel_impl);

            // Start RPC system
            let rpc_system = RpcSystem::new(Box::new(network), Some(kernel_client.clone().client));

            // Spawn the RPC system
            tokio::task::spawn_local(async move {
                if let Err(e) = rpc_system.await {
                    tracing::error!("RPC error: {}", e);
                }
            });
        }
    }
}

/// Implementation of the Kernel Cap'n Proto interface.
struct KernelImpl {
    kernel: Arc<Kernel>,
}

impl KernelImpl {
    fn new(kernel: Arc<Kernel>) -> Self {
        Self { kernel }
    }
}

impl kernel::Server for KernelImpl {
    /// Execute kaish code and return the result.
    fn execute(
        &mut self,
        params: kernel::ExecuteParams,
        mut results: kernel::ExecuteResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let kernel = self.kernel.clone();

        let input = match params.get() {
            Ok(p) => match p.get_input() {
                Ok(s) => match s.to_str() {
                    Ok(s) => s.to_string(),
                    Err(e) => return capnp::capability::Promise::err(capnp::Error::failed(format!("invalid utf8: {}", e))),
                },
                Err(e) => return capnp::capability::Promise::err(e),
            },
            Err(e) => return capnp::capability::Promise::err(e),
        };

        capnp::capability::Promise::from_future(async move {
            let exec_result = kernel.execute(&input).await.map_err(|e| {
                capnp::Error::failed(format!("execution error: {}", e))
            })?;

            // Build the result
            let mut result_builder = results.get().init_result();
            result_builder.set_code(exec_result.code as i32);
            result_builder.set_ok(exec_result.ok());
            result_builder.set_err(&exec_result.err);
            result_builder.set_stdout(exec_result.out.as_bytes());
            result_builder.set_stderr(b"");

            // Set data if present
            if let Some(data) = &exec_result.data {
                set_value(&mut result_builder.reborrow().init_data(), data);
            }

            Ok(())
        })
    }

    /// Get a variable value.
    fn get_var(
        &mut self,
        params: kernel::GetVarParams,
        mut results: kernel::GetVarResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let kernel = self.kernel.clone();

        let name = match params.get() {
            Ok(p) => match p.get_name() {
                Ok(s) => match s.to_str() {
                    Ok(s) => s.to_string(),
                    Err(e) => return capnp::capability::Promise::err(capnp::Error::failed(format!("invalid utf8: {}", e))),
                },
                Err(e) => return capnp::capability::Promise::err(e),
            },
            Err(e) => return capnp::capability::Promise::err(e),
        };

        capnp::capability::Promise::from_future(async move {
            if let Some(value) = kernel.get_var(&name).await {
                set_value(&mut results.get().init_value(), &value);
            }

            Ok(())
        })
    }

    /// Set a variable value.
    fn set_var(
        &mut self,
        params: kernel::SetVarParams,
        _results: kernel::SetVarResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let kernel = self.kernel.clone();

        let (name, value) = match params.get() {
            Ok(p) => {
                let name = match p.get_name() {
                    Ok(s) => match s.to_str() {
                        Ok(s) => s.to_string(),
                        Err(e) => return capnp::capability::Promise::err(capnp::Error::failed(format!("invalid utf8: {}", e))),
                    },
                    Err(e) => return capnp::capability::Promise::err(e),
                };
                let value_reader = match p.get_value() {
                    Ok(v) => v,
                    Err(e) => return capnp::capability::Promise::err(e),
                };
                let value = match read_value(&value_reader) {
                    Ok(v) => v,
                    Err(e) => return capnp::capability::Promise::err(e),
                };
                (name, value)
            }
            Err(e) => return capnp::capability::Promise::err(e),
        };

        capnp::capability::Promise::from_future(async move {
            kernel.set_var(&name, value).await;
            Ok(())
        })
    }

    /// List all variables.
    fn list_vars(
        &mut self,
        _params: kernel::ListVarsParams,
        mut results: kernel::ListVarsResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let kernel = self.kernel.clone();

        capnp::capability::Promise::from_future(async move {
            let vars = kernel.list_vars().await;

            let mut list = results.get().init_vars(vars.len() as u32);
            for (i, (name, value)) in vars.into_iter().enumerate() {
                let mut entry = list.reborrow().get(i as u32);
                entry.set_key(&name);
                set_value(&mut entry.init_value(), &value);
            }

            Ok(())
        })
    }

    /// List available tools.
    fn list_tools(
        &mut self,
        _params: kernel::ListToolsParams,
        mut results: kernel::ListToolsResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let schemas = self.kernel.tool_schemas();

        let mut list = results.get().init_tools(schemas.len() as u32);
        for (i, schema) in schemas.into_iter().enumerate() {
            let mut entry = list.reborrow().get(i as u32);
            entry.set_name(&schema.name);
            entry.set_description(&schema.description);
            // source defaults to builtin
        }

        capnp::capability::Promise::ok(())
    }

    /// Ping the kernel (health check).
    fn ping(
        &mut self,
        _params: kernel::PingParams,
        mut results: kernel::PingResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        results.get().set_pong("pong");
        capnp::capability::Promise::ok(())
    }

    /// Shutdown the kernel.
    ///
    /// Currently logs the request and returns success. Actual process termination
    /// is handled by the calling frontend (REPL, MCP server) after receiving the
    /// successful response.
    fn shutdown(
        &mut self,
        _params: kernel::ShutdownParams,
        _results: kernel::ShutdownResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        tracing::info!("Kernel shutdown requested via RPC");
        capnp::capability::Promise::ok(())
    }

    /// Reset kernel state.
    fn reset(
        &mut self,
        _params: kernel::ResetParams,
        _results: kernel::ResetResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let kernel = self.kernel.clone();

        capnp::capability::Promise::from_future(async move {
            kernel.reset().await.map_err(|e| {
                capnp::Error::failed(format!("reset error: {}", e))
            })?;
            Ok(())
        })
    }

    // --- Placeholder implementations for remaining methods ---

    fn execute_streaming(
        &mut self,
        _params: kernel::ExecuteStreamingParams,
        _results: kernel::ExecuteStreamingResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "execute_streaming not yet implemented".into(),
        ))
    }

    fn call_tool(
        &mut self,
        _params: kernel::CallToolParams,
        _results: kernel::CallToolResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "call_tool not yet implemented".into(),
        ))
    }

    fn mount(
        &mut self,
        _params: kernel::MountParams,
        _results: kernel::MountResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "mount not yet implemented".into(),
        ))
    }

    fn unmount(
        &mut self,
        _params: kernel::UnmountParams,
        _results: kernel::UnmountResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "unmount not yet implemented".into(),
        ))
    }

    fn list_mounts(
        &mut self,
        _params: kernel::ListMountsParams,
        _results: kernel::ListMountsResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "list_mounts not yet implemented".into(),
        ))
    }

    fn register_mcp(
        &mut self,
        _params: kernel::RegisterMcpParams,
        _results: kernel::RegisterMcpResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "register_mcp not yet implemented".into(),
        ))
    }

    fn unregister_mcp(
        &mut self,
        _params: kernel::UnregisterMcpParams,
        _results: kernel::UnregisterMcpResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "unregister_mcp not yet implemented".into(),
        ))
    }

    fn list_mcp_servers(
        &mut self,
        _params: kernel::ListMcpServersParams,
        _results: kernel::ListMcpServersResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "list_mcp_servers not yet implemented".into(),
        ))
    }

    fn snapshot(
        &mut self,
        _params: kernel::SnapshotParams,
        _results: kernel::SnapshotResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "snapshot not yet implemented".into(),
        ))
    }

    fn restore(
        &mut self,
        _params: kernel::RestoreParams,
        _results: kernel::RestoreResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "restore not yet implemented".into(),
        ))
    }

    fn read_blob(
        &mut self,
        _params: kernel::ReadBlobParams,
        _results: kernel::ReadBlobResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "read_blob not yet implemented".into(),
        ))
    }

    fn write_blob(
        &mut self,
        _params: kernel::WriteBlobParams,
        _results: kernel::WriteBlobResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "write_blob not yet implemented".into(),
        ))
    }

    fn delete_blob(
        &mut self,
        _params: kernel::DeleteBlobParams,
        _results: kernel::DeleteBlobResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "delete_blob not yet implemented".into(),
        ))
    }
}

// ============================================================
// Value Conversion Helpers
// ============================================================

use kaish_schema::value;
use crate::ast::Value;

/// Convert a kaish Value to a Cap'n Proto Value.
fn set_value(builder: &mut value::Builder<'_>, value: &Value) {
    match value {
        Value::Null => builder.set_null(()),
        Value::Bool(b) => builder.set_bool(*b),
        Value::Int(i) => builder.set_int(*i),
        Value::Float(f) => builder.set_float(*f),
        Value::String(s) => builder.set_string(s),
    }
}

/// Read a kaish Value from a Cap'n Proto Value.
///
/// Arrays and objects are serialized as JSON strings.
fn read_value(reader: &value::Reader<'_>) -> Result<Value, capnp::Error> {
    use value::Which;
    match reader.which()? {
        Which::Null(()) => Ok(Value::Null),
        Which::Bool(b) => Ok(Value::Bool(b)),
        Which::Int(i) => Ok(Value::Int(i)),
        Which::Float(f) => Ok(Value::Float(f)),
        Which::String(s) => {
            let text = s?;
            let string = text.to_str().map_err(|e| capnp::Error::failed(format!("invalid utf8: {}", e)))?;
            Ok(Value::String(string.to_string()))
        }
        Which::Array(arr) => {
            // Convert array to JSON string
            let arr = arr?;
            let items: Result<Vec<_>, _> = arr.iter().map(|v| read_value_to_json(&v)).collect();
            let json_array = serde_json::Value::Array(items?);
            Ok(Value::String(json_array.to_string()))
        }
        Which::Object(obj) => {
            // Convert object to JSON string
            let obj = obj?;
            let mut map = serde_json::Map::new();
            for kv in obj.iter() {
                let key_text = kv.get_key()?;
                let key = key_text.to_str().map_err(|e| capnp::Error::failed(format!("invalid utf8: {}", e)))?.to_string();
                let val = read_value_to_json(&kv.get_value()?)?;
                map.insert(key, val);
            }
            Ok(Value::String(serde_json::Value::Object(map).to_string()))
        }
        Which::Blob(_) => {
            // Blobs are not directly representable as Value
            Err(capnp::Error::failed("blob values not supported".into()))
        }
    }
}

/// Helper to convert Cap'n Proto Value to serde_json::Value
fn read_value_to_json(reader: &value::Reader<'_>) -> Result<serde_json::Value, capnp::Error> {
    use value::Which;
    match reader.which()? {
        Which::Null(()) => Ok(serde_json::Value::Null),
        Which::Bool(b) => Ok(serde_json::Value::Bool(b)),
        Which::Int(i) => Ok(serde_json::Value::Number(i.into())),
        Which::Float(f) => Ok(serde_json::Number::from_f64(f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null)),
        Which::String(s) => {
            let text = s?;
            let string = text.to_str().map_err(|e| capnp::Error::failed(format!("invalid utf8: {}", e)))?;
            Ok(serde_json::Value::String(string.to_string()))
        }
        Which::Array(arr) => {
            let arr = arr?;
            let items: Result<Vec<_>, _> = arr.iter().map(|v| read_value_to_json(&v)).collect();
            Ok(serde_json::Value::Array(items?))
        }
        Which::Object(obj) => {
            let obj = obj?;
            let mut map = serde_json::Map::new();
            for kv in obj.iter() {
                let key_text = kv.get_key()?;
                let key = key_text.to_str().map_err(|e| capnp::Error::failed(format!("invalid utf8: {}", e)))?.to_string();
                let val = read_value_to_json(&kv.get_value()?)?;
                map.insert(key, val);
            }
            Ok(serde_json::Value::Object(map))
        }
        Which::Blob(_) => Err(capnp::Error::failed("blob values not supported".into())),
    }
}
