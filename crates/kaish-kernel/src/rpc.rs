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

use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

use anyhow::{Context, Result};
use capnp_rpc::{rpc_twoparty_capnp, twoparty, RpcSystem};
use futures::AsyncReadExt;
use tokio::net::UnixListener;
use tokio::sync::Mutex;
use tokio_util::compat::TokioAsyncReadCompatExt;

use kaish_schema::{blob_sink, blob_stream, kernel};

use crate::kernel::Kernel;
use crate::paths as state_paths;
use crate::vfs::{Filesystem, VfsRouter};

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

// ============================================================
// Blob Stream/Sink Implementations
// ============================================================

/// Implementation of BlobStream for reading blobs.
struct BlobStreamImpl {
    data: Vec<u8>,
    position: Mutex<usize>,
}

impl BlobStreamImpl {
    fn new(data: Vec<u8>) -> Self {
        Self {
            data,
            position: Mutex::new(0),
        }
    }
}

#[allow(refining_impl_trait)]
impl blob_stream::Server for BlobStreamImpl {
    fn read(
        self: Rc<Self>,
        params: blob_stream::ReadParams,
        mut results: blob_stream::ReadResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let max_bytes = match params.get() {
            Ok(p) => p.get_max_bytes() as usize,
            Err(e) => return capnp::capability::Promise::err(e),
        };

        // Use block_in_place to safely access the mutex in sync context
        let (chunk, done) = tokio::task::block_in_place(|| {
            let mut pos = self.position.blocking_lock();
            let remaining = self.data.len().saturating_sub(*pos);
            let read_size = max_bytes.min(remaining);
            let chunk = self.data[*pos..*pos + read_size].to_vec();
            *pos += read_size;
            let done = *pos >= self.data.len();
            (chunk, done)
        });

        let mut builder = results.get();
        builder.set_data(&chunk);
        builder.set_done(done);
        capnp::capability::Promise::ok(())
    }

    fn size(
        self: Rc<Self>,
        _params: blob_stream::SizeParams,
        mut results: blob_stream::SizeResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let mut builder = results.get();
        builder.set_bytes(self.data.len() as u64);
        builder.set_known(true);
        capnp::capability::Promise::ok(())
    }

    fn cancel(
        self: Rc<Self>,
        _params: blob_stream::CancelParams,
        _results: blob_stream::CancelResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        // Nothing to clean up - data is dropped when impl is dropped
        capnp::capability::Promise::ok(())
    }
}

/// Implementation of BlobSink for writing blobs.
struct BlobSinkImpl {
    vfs: Arc<VfsRouter>,
    path: PathBuf,
    data: Mutex<Vec<u8>>,
    aborted: Mutex<bool>,
}

impl BlobSinkImpl {
    fn new(vfs: Arc<VfsRouter>, path: PathBuf) -> Self {
        Self {
            vfs,
            path,
            data: Mutex::new(Vec::new()),
            aborted: Mutex::new(false),
        }
    }
}

#[allow(refining_impl_trait)]
impl blob_sink::Server for BlobSinkImpl {
    fn write(
        self: Rc<Self>,
        params: blob_sink::WriteParams,
        _results: blob_sink::WriteResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let chunk = match params.get() {
            Ok(p) => match p.get_data() {
                Ok(d) => d.to_vec(),
                Err(e) => return capnp::capability::Promise::err(e),
            },
            Err(e) => return capnp::capability::Promise::err(e),
        };

        tokio::task::block_in_place(|| {
            let aborted = self.aborted.blocking_lock();
            if *aborted {
                return;
            }
            drop(aborted);

            let mut data = self.data.blocking_lock();
            data.extend(chunk);
        });

        capnp::capability::Promise::ok(())
    }

    fn finish(
        self: Rc<Self>,
        _params: blob_sink::FinishParams,
        mut results: blob_sink::FinishResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let vfs = self.vfs.clone();
        let path = self.path.clone();

        // Get the data and compute hash
        let (data, hash) = tokio::task::block_in_place(|| {
            let aborted = self.aborted.blocking_lock();
            if *aborted {
                return (Vec::new(), Vec::new());
            }
            drop(aborted);

            let data = self.data.blocking_lock().clone();

            // Compute SHA-256 hash
            use std::collections::hash_map::DefaultHasher;
            use std::hash::{Hash, Hasher};
            let mut hasher = DefaultHasher::new();
            data.hash(&mut hasher);
            let hash_value = hasher.finish();
            let hash = hash_value.to_be_bytes().to_vec();

            (data, hash)
        });

        capnp::capability::Promise::from_future(async move {
            // Ensure parent directory exists
            let parent = path.parent().unwrap_or(Path::new("/v/blobs"));
            if let Err(e) = vfs.mkdir(parent).await {
                // Ignore "already exists" errors
                if e.kind() != std::io::ErrorKind::AlreadyExists {
                    tracing::warn!("Failed to create blob directory: {}", e);
                }
            }

            // Write the blob
            vfs.write(&path, &data).await.map_err(|e| {
                capnp::Error::failed(format!("failed to write blob: {}", e))
            })?;

            results.get().set_hash(&hash);
            Ok(())
        })
    }

    fn abort(
        self: Rc<Self>,
        _params: blob_sink::AbortParams,
        _results: blob_sink::AbortResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        tokio::task::block_in_place(|| {
            let mut aborted = self.aborted.blocking_lock();
            *aborted = true;
        });
        capnp::capability::Promise::ok(())
    }
}

/// Generate a unique blob ID.
fn generate_blob_id() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    use std::sync::atomic::{AtomicU64, Ordering};

    static COUNTER: AtomicU64 = AtomicU64::new(0);

    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let count = COUNTER.fetch_add(1, Ordering::SeqCst);

    format!("{:x}-{:x}", timestamp, count)
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

#[allow(refining_impl_trait)]
impl kernel::Server for KernelImpl {
    /// Execute kaish code and return the result.
    fn execute(
        self: Rc<Self>,
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

            // Set structured output data
            if let Some(ref output) = exec_result.output {
                set_output_data(&mut result_builder.reborrow().init_output(), output);
            }

            Ok(())
        })
    }

    /// Get a variable value.
    fn get_var(
        self: Rc<Self>,
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
        self: Rc<Self>,
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
        self: Rc<Self>,
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
        self: Rc<Self>,
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
        self: Rc<Self>,
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
        self: Rc<Self>,
        _params: kernel::ShutdownParams,
        _results: kernel::ShutdownResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        tracing::info!("Kernel shutdown requested via RPC");
        capnp::capability::Promise::ok(())
    }

    /// Reset kernel state.
    fn reset(
        self: Rc<Self>,
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

    // --- Working Directory ---

    /// Get the current working directory.
    fn get_cwd(
        self: Rc<Self>,
        _params: kernel::GetCwdParams,
        mut results: kernel::GetCwdResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let kernel = self.kernel.clone();

        capnp::capability::Promise::from_future(async move {
            let cwd = kernel.cwd().await;
            results.get().set_path(cwd.to_string_lossy());
            Ok(())
        })
    }

    /// Set the current working directory.
    fn set_cwd(
        self: Rc<Self>,
        params: kernel::SetCwdParams,
        mut results: kernel::SetCwdResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let kernel = self.kernel.clone();

        let path = match params.get() {
            Ok(p) => match p.get_path() {
                Ok(s) => match s.to_str() {
                    Ok(s) => s.to_string(),
                    Err(e) => return capnp::capability::Promise::err(capnp::Error::failed(format!("invalid utf8: {}", e))),
                },
                Err(e) => return capnp::capability::Promise::err(e),
            },
            Err(e) => return capnp::capability::Promise::err(e),
        };

        capnp::capability::Promise::from_future(async move {
            kernel.set_cwd(std::path::PathBuf::from(&path)).await;
            let mut r = results.get();
            r.set_success(true);
            r.set_error("");
            Ok(())
        })
    }

    // --- Last Result ---

    /// Get the last execution result ($?).
    fn get_last_result(
        self: Rc<Self>,
        _params: kernel::GetLastResultParams,
        mut results: kernel::GetLastResultResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let kernel = self.kernel.clone();

        capnp::capability::Promise::from_future(async move {
            let exec_result = kernel.last_result().await;

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

            // Set structured output data
            if let Some(ref output) = exec_result.output {
                set_output_data(&mut result_builder.reborrow().init_output(), output);
            }

            Ok(())
        })
    }

    // --- Placeholder implementations for remaining methods ---

    fn execute_streaming(
        self: Rc<Self>,
        _params: kernel::ExecuteStreamingParams,
        _results: kernel::ExecuteStreamingResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "execute_streaming not yet implemented".into(),
        ))
    }

    fn call_tool(
        self: Rc<Self>,
        _params: kernel::CallToolParams,
        _results: kernel::CallToolResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "call_tool not yet implemented".into(),
        ))
    }

    fn mount(
        self: Rc<Self>,
        _params: kernel::MountParams,
        _results: kernel::MountResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "mount not yet implemented".into(),
        ))
    }

    fn unmount(
        self: Rc<Self>,
        _params: kernel::UnmountParams,
        _results: kernel::UnmountResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "unmount not yet implemented".into(),
        ))
    }

    fn list_mounts(
        self: Rc<Self>,
        _params: kernel::ListMountsParams,
        _results: kernel::ListMountsResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "list_mounts not yet implemented".into(),
        ))
    }

    fn register_mcp(
        self: Rc<Self>,
        _params: kernel::RegisterMcpParams,
        _results: kernel::RegisterMcpResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "register_mcp not yet implemented".into(),
        ))
    }

    fn unregister_mcp(
        self: Rc<Self>,
        _params: kernel::UnregisterMcpParams,
        _results: kernel::UnregisterMcpResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "unregister_mcp not yet implemented".into(),
        ))
    }

    fn list_mcp_servers(
        self: Rc<Self>,
        _params: kernel::ListMcpServersParams,
        _results: kernel::ListMcpServersResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "list_mcp_servers not yet implemented".into(),
        ))
    }

    fn snapshot(
        self: Rc<Self>,
        _params: kernel::SnapshotParams,
        _results: kernel::SnapshotResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "snapshot not yet implemented".into(),
        ))
    }

    fn restore(
        self: Rc<Self>,
        _params: kernel::RestoreParams,
        _results: kernel::RestoreResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        capnp::capability::Promise::err(capnp::Error::unimplemented(
            "restore not yet implemented".into(),
        ))
    }

    fn read_blob(
        self: Rc<Self>,
        params: kernel::ReadBlobParams,
        mut results: kernel::ReadBlobResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let vfs = self.kernel.vfs();

        let id = match params.get() {
            Ok(p) => match p.get_id() {
                Ok(s) => match s.to_str() {
                    Ok(s) => s.to_string(),
                    Err(e) => return capnp::capability::Promise::err(capnp::Error::failed(format!("invalid utf8: {}", e))),
                },
                Err(e) => return capnp::capability::Promise::err(e),
            },
            Err(e) => return capnp::capability::Promise::err(e),
        };

        capnp::capability::Promise::from_future(async move {
            let path = PathBuf::from(format!("/v/blobs/{}", id));

            // Read the blob data
            let data = vfs.read(&path).await.map_err(|e| {
                capnp::Error::failed(format!("failed to read blob {}: {}", id, e))
            })?;

            // Create the stream implementation
            let stream_impl = BlobStreamImpl::new(data);
            let stream_client: blob_stream::Client = capnp_rpc::new_client(stream_impl);

            results.get().set_stream(stream_client);
            Ok(())
        })
    }

    fn write_blob(
        self: Rc<Self>,
        params: kernel::WriteBlobParams,
        mut results: kernel::WriteBlobResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let vfs = self.kernel.vfs();

        let (content_type, _size) = match params.get() {
            Ok(p) => {
                let ct = match p.get_content_type() {
                    Ok(s) => match s.to_str() {
                        Ok(s) => s.to_string(),
                        Err(e) => return capnp::capability::Promise::err(capnp::Error::failed(format!("invalid utf8: {}", e))),
                    },
                    Err(e) => return capnp::capability::Promise::err(e),
                };
                let size = p.get_size();
                (ct, size)
            },
            Err(e) => return capnp::capability::Promise::err(e),
        };

        // Generate a unique blob ID
        let id = generate_blob_id();
        let path = PathBuf::from(format!("/v/blobs/{}", id));

        // Store content type as metadata (could be extended later)
        tracing::debug!("Creating blob {} with content type {}", id, content_type);

        // Create the sink implementation
        let sink_impl = BlobSinkImpl::new(vfs, path);
        let sink_client: blob_sink::Client = capnp_rpc::new_client(sink_impl);

        let mut builder = results.get();
        builder.set_id(&id);
        builder.set_stream(sink_client);

        capnp::capability::Promise::ok(())
    }

    fn delete_blob(
        self: Rc<Self>,
        params: kernel::DeleteBlobParams,
        mut results: kernel::DeleteBlobResults,
    ) -> capnp::capability::Promise<(), capnp::Error> {
        let vfs = self.kernel.vfs();

        let id = match params.get() {
            Ok(p) => match p.get_id() {
                Ok(s) => match s.to_str() {
                    Ok(s) => s.to_string(),
                    Err(e) => return capnp::capability::Promise::err(capnp::Error::failed(format!("invalid utf8: {}", e))),
                },
                Err(e) => return capnp::capability::Promise::err(e),
            },
            Err(e) => return capnp::capability::Promise::err(e),
        };

        capnp::capability::Promise::from_future(async move {
            let path = PathBuf::from(format!("/v/blobs/{}", id));

            // Delete the blob
            let success = match vfs.remove(&path).await {
                Ok(()) => true,
                Err(e) => {
                    tracing::warn!("Failed to delete blob {}: {}", id, e);
                    false
                }
            };

            results.get().set_success(success);
            Ok(())
        })
    }
}

// ============================================================
// Value Conversion Helpers
// ============================================================

use kaish_schema::value;
use kaish_schema::{output_data, output_node};
use crate::ast::Value;
use crate::interpreter::{EntryType, OutputData, OutputNode};

/// Convert a kaish Value to a Cap'n Proto Value.
fn set_value(builder: &mut value::Builder<'_>, value: &Value) {
    match value {
        Value::Null => builder.set_null(()),
        Value::Bool(b) => builder.set_bool(*b),
        Value::Int(i) => builder.set_int(*i),
        Value::Float(f) => builder.set_float(*f),
        Value::String(s) => builder.set_string(s),
        Value::Json(json) => {
            // Serialize Json values using the Cap'n Proto array/object types
            set_json_value(builder.reborrow(), json);
        }
        Value::Blob(blob) => {
            let mut blob_builder = builder.reborrow().init_blob();
            blob_builder.set_id(&blob.id);
            blob_builder.set_size(blob.size);
            blob_builder.set_content_type(&blob.content_type);
            if let Some(hash) = &blob.hash {
                blob_builder.set_hash(hash);
            }
        }
    }
}

/// Helper to serialize serde_json::Value to Cap'n Proto.
fn set_json_value(mut builder: value::Builder<'_>, json: &serde_json::Value) {
    match json {
        serde_json::Value::Null => builder.set_null(()),
        serde_json::Value::Bool(b) => builder.set_bool(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                builder.set_int(i);
            } else if let Some(f) = n.as_f64() {
                builder.set_float(f);
            } else {
                builder.set_string(n.to_string());
            }
        }
        serde_json::Value::String(s) => builder.set_string(s),
        serde_json::Value::Array(arr) => {
            let mut array_builder = builder.init_array(arr.len() as u32);
            for (i, item) in arr.iter().enumerate() {
                set_json_value(array_builder.reborrow().get(i as u32), item);
            }
        }
        serde_json::Value::Object(obj) => {
            let mut object_builder = builder.init_object(obj.len() as u32);
            for (i, (key, val)) in obj.iter().enumerate() {
                let mut entry = object_builder.reborrow().get(i as u32);
                entry.set_key(key);
                set_json_value(entry.init_value(), val);
            }
        }
    }
}

/// Convert a kaish OutputData to Cap'n Proto OutputData.
fn set_output_data(builder: &mut output_data::Builder<'_>, output: &OutputData) {
    // Set headers if present
    if let Some(ref headers) = output.headers {
        let mut headers_builder = builder.reborrow().init_headers(headers.len() as u32);
        for (i, header) in headers.iter().enumerate() {
            headers_builder.set(i as u32, header);
        }
    }

    // Set root nodes
    let mut root_builder = builder.reborrow().init_root(output.root.len() as u32);
    for (i, node) in output.root.iter().enumerate() {
        let mut node_builder = root_builder.reborrow().get(i as u32);
        set_output_node(&mut node_builder, node);
    }
}

/// Convert a kaish OutputNode to Cap'n Proto OutputNode.
fn set_output_node(builder: &mut output_node::Builder<'_>, node: &OutputNode) {
    builder.set_name(&node.name);

    // Set entry type
    use kaish_schema::kaish_capnp::EntryType as SchemaEntryType;
    let entry_type = match node.entry_type {
        EntryType::Text => SchemaEntryType::Text,
        EntryType::File => SchemaEntryType::File,
        EntryType::Directory => SchemaEntryType::Directory,
        EntryType::Executable => SchemaEntryType::Executable,
        EntryType::Symlink => SchemaEntryType::Symlink,
    };
    builder.set_entry_type(entry_type);

    // Set text if present
    if let Some(ref text) = node.text {
        builder.set_text(text);
    }

    // Set cells
    if !node.cells.is_empty() {
        let mut cells_builder = builder.reborrow().init_cells(node.cells.len() as u32);
        for (i, cell) in node.cells.iter().enumerate() {
            cells_builder.set(i as u32, cell);
        }
    }

    // Set children recursively
    if !node.children.is_empty() {
        let mut children_builder = builder.reborrow().init_children(node.children.len() as u32);
        for (i, child) in node.children.iter().enumerate() {
            let mut child_builder = children_builder.reborrow().get(i as u32);
            set_output_node(&mut child_builder, child);
        }
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
        Which::Blob(blob) => {
            // Convert blob reference to JSON string representation
            let blob = blob?;
            let id = blob.get_id()?.to_str().map_err(|e| capnp::Error::failed(format!("invalid utf8: {}", e)))?;
            let size = blob.get_size();
            let content_type = blob.get_content_type()?.to_str().map_err(|e| capnp::Error::failed(format!("invalid utf8: {}", e)))?;
            let hash = blob.get_hash()?;

            let mut map = serde_json::Map::new();
            map.insert("_type".to_string(), serde_json::Value::String("blob".to_string()));
            map.insert("id".to_string(), serde_json::Value::String(id.to_string()));
            map.insert("size".to_string(), serde_json::Value::Number(size.into()));
            map.insert("contentType".to_string(), serde_json::Value::String(content_type.to_string()));
            if !hash.is_empty() {
                let hash_hex: String = hash.iter().map(|b| format!("{:02x}", b)).collect();
                map.insert("hash".to_string(), serde_json::Value::String(hash_hex));
            }
            Ok(Value::String(serde_json::Value::Object(map).to_string()))
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
        Which::Blob(blob) => {
            // Convert blob reference to JSON object
            let blob = blob?;
            let id = blob.get_id()?.to_str().map_err(|e| capnp::Error::failed(format!("invalid utf8: {}", e)))?;
            let size = blob.get_size();
            let content_type = blob.get_content_type()?.to_str().map_err(|e| capnp::Error::failed(format!("invalid utf8: {}", e)))?;
            let hash = blob.get_hash()?;

            let mut map = serde_json::Map::new();
            map.insert("_type".to_string(), serde_json::Value::String("blob".to_string()));
            map.insert("id".to_string(), serde_json::Value::String(id.to_string()));
            map.insert("size".to_string(), serde_json::Value::Number(size.into()));
            map.insert("contentType".to_string(), serde_json::Value::String(content_type.to_string()));
            if !hash.is_empty() {
                let hash_hex: String = hash.iter().map(|b| format!("{:02x}", b)).collect();
                map.insert("hash".to_string(), serde_json::Value::String(hash_hex));
            }
            Ok(serde_json::Value::Object(map))
        }
    }
}
