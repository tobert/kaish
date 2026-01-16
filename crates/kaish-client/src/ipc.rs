//! IPC client for connecting to remote kernels via Unix socket.
//!
//! The `IpcClient` connects to a kaish kernel server using Cap'n Proto RPC
//! over a Unix domain socket. This is ideal for:
//!
//! - CLI tools that connect to a running kernel
//! - Multi-process architectures
//! - Remote kernel access

use std::path::Path;

use async_trait::async_trait;
use capnp_rpc::{rpc_twoparty_capnp, twoparty, RpcSystem};
use futures::AsyncReadExt;
use tokio::net::UnixStream;
use tokio_util::compat::TokioAsyncReadCompatExt;

use kaish_kernel::ast::Value;
use kaish_kernel::interpreter::ExecResult;
use kaish_schema::kernel;

use crate::traits::{ClientError, ClientResult, KernelClient};

/// A client that connects to a remote kernel via Unix socket.
///
/// # Example
///
/// ```ignore
/// use kaish_client::IpcClient;
///
/// let client = IpcClient::connect("/run/user/1000/kaish/default.sock").await?;
/// let result = client.execute("echo hello").await?;
/// assert!(result.ok());
/// ```
pub struct IpcClient {
    client: kernel::Client,
    // Keep the RPC system alive
    #[allow(dead_code)]
    disconnector: capnp_rpc::Disconnector<rpc_twoparty_capnp::Side>,
}

impl IpcClient {
    /// Connect to a kernel at the given socket path.
    pub async fn connect(socket_path: impl AsRef<Path>) -> ClientResult<Self> {
        let socket_path = socket_path.as_ref();

        let stream = UnixStream::connect(socket_path)
            .await
            .map_err(|e| ClientError::Connection(format!(
                "failed to connect to {}: {}",
                socket_path.display(),
                e
            )))?;

        let stream = stream.compat();
        let (reader, writer) = stream.split();

        let network = twoparty::VatNetwork::new(
            reader,
            writer,
            rpc_twoparty_capnp::Side::Client,
            Default::default(),
        );

        let mut rpc_system = RpcSystem::new(Box::new(network), None);
        let client: kernel::Client = rpc_system.bootstrap(rpc_twoparty_capnp::Side::Server);
        let disconnector = rpc_system.get_disconnector();

        // Spawn the RPC system
        tokio::task::spawn_local(async move {
            if let Err(e) = rpc_system.await {
                tracing::error!("RPC system error: {}", e);
            }
        });

        Ok(Self { client, disconnector })
    }

    /// Connect to the default kernel socket.
    ///
    /// Uses `$XDG_RUNTIME_DIR/kaish/default.sock`
    pub async fn connect_default() -> ClientResult<Self> {
        let socket_path = kaish_kernel::state::paths::runtime_dir().join("default.sock");
        Self::connect(&socket_path).await
    }
}

#[async_trait(?Send)]
impl KernelClient for IpcClient {
    async fn execute(&self, input: &str) -> ClientResult<ExecResult> {
        let mut request = self.client.execute_request();
        request.get().set_input(input);

        let response = request.send().promise.await?;
        let result = response.get()?.get_result()?;

        Ok(ExecResult {
            code: result.get_code() as i64,
            out: String::from_utf8_lossy(result.get_stdout()?).to_string(),
            err: result.get_err()?.to_str()?.to_string(),
            data: if result.has_data() {
                Some(read_value(&result.get_data()?)?)
            } else {
                None
            },
        })
    }

    async fn get_var(&self, name: &str) -> ClientResult<Option<Value>> {
        let mut request = self.client.get_var_request();
        request.get().set_name(name);

        let response = request.send().promise.await?;
        let result = response.get()?;

        if result.has_value() {
            Ok(Some(read_value(&result.get_value()?)?))
        } else {
            Ok(None)
        }
    }

    async fn set_var(&self, name: &str, value: Value) -> ClientResult<()> {
        let mut request = self.client.set_var_request();
        {
            let mut params = request.get();
            params.set_name(name);
            set_value(&mut params.init_value(), &value);
        }

        request.send().promise.await?;
        Ok(())
    }

    async fn list_vars(&self) -> ClientResult<Vec<(String, Value)>> {
        let request = self.client.list_vars_request();
        let response = request.send().promise.await?;
        let result = response.get()?;

        let vars = result.get_vars()?;
        let mut out = Vec::with_capacity(vars.len() as usize);

        for kv in vars.iter() {
            let key = kv.get_key()?.to_str()?.to_string();
            let value = read_value(&kv.get_value()?)?;
            out.push((key, value));
        }

        Ok(out)
    }

    async fn cwd(&self) -> ClientResult<String> {
        // CWD isn't directly exposed in the RPC interface yet
        // Execute pwd to get it
        let result = self.execute("pwd").await?;
        Ok(result.out.trim().to_string())
    }

    async fn set_cwd(&self, path: &str) -> ClientResult<()> {
        // Use cd command
        self.execute(&format!("cd {}", path)).await?;
        Ok(())
    }

    async fn last_result(&self) -> ClientResult<ExecResult> {
        // Execute a no-op to get $?
        // This is a workaround since last_result isn't in the RPC interface
        self.execute("true").await
    }

    async fn reset(&self) -> ClientResult<()> {
        let request = self.client.reset_request();
        request.send().promise.await?;
        Ok(())
    }

    async fn ping(&self) -> ClientResult<String> {
        let request = self.client.ping_request();
        let response = request.send().promise.await?;
        let pong = response.get()?.get_pong()?.to_str()?;
        Ok(pong.to_string())
    }

    async fn shutdown(&self) -> ClientResult<()> {
        let request = self.client.shutdown_request();
        request.send().promise.await?;
        Ok(())
    }
}

// ============================================================
// Value Conversion Helpers
// ============================================================

use kaish_schema::value;
use kaish_kernel::ast::Expr;

/// Convert a kaish Value to a Cap'n Proto Value.
fn set_value(builder: &mut value::Builder<'_>, value: &Value) {
    match value {
        Value::Null => builder.set_null(()),
        Value::Bool(b) => builder.set_bool(*b),
        Value::Int(i) => builder.set_int(*i),
        Value::Float(f) => builder.set_float(*f),
        Value::String(s) => builder.set_string(s),
        Value::Array(items) => {
            let mut list = builder.reborrow().init_array(items.len() as u32);
            for (i, item) in items.iter().enumerate() {
                if let Expr::Literal(v) = item {
                    set_value(&mut list.reborrow().get(i as u32), v);
                }
            }
        }
        Value::Object(fields) => {
            let mut list = builder.reborrow().init_object(fields.len() as u32);
            for (i, (key, val_expr)) in fields.iter().enumerate() {
                let mut entry = list.reborrow().get(i as u32);
                entry.set_key(key);
                if let Expr::Literal(v) = val_expr {
                    set_value(&mut entry.init_value(), v);
                }
            }
        }
    }
}

/// Read a kaish Value from a Cap'n Proto Value.
fn read_value(reader: &value::Reader<'_>) -> ClientResult<Value> {
    use value::Which;
    match reader.which()? {
        Which::Null(()) => Ok(Value::Null),
        Which::Bool(b) => Ok(Value::Bool(b)),
        Which::Int(i) => Ok(Value::Int(i)),
        Which::Float(f) => Ok(Value::Float(f)),
        Which::String(s) => {
            let text = s?;
            let string = text.to_str().map_err(|e| {
                ClientError::Rpc(capnp::Error::failed(format!("invalid utf8: {}", e)))
            })?;
            Ok(Value::String(string.to_string()))
        }
        Which::Array(arr) => {
            let arr = arr?;
            let items: Result<Vec<_>, _> = arr
                .iter()
                .map(|v| read_value(&v).map(|v| Expr::Literal(v)))
                .collect();
            Ok(Value::Array(items?))
        }
        Which::Object(obj) => {
            let obj = obj?;
            let mut fields = Vec::new();
            for kv in obj.iter() {
                let key_text = kv.get_key()?;
                let key = key_text.to_str().map_err(|e| {
                    ClientError::Rpc(capnp::Error::failed(format!("invalid utf8: {}", e)))
                })?.to_string();
                let val = read_value(&kv.get_value()?)?;
                fields.push((key, Expr::Literal(val)));
            }
            Ok(Value::Object(fields))
        }
        Which::Blob(_) => {
            Err(ClientError::Rpc(capnp::Error::failed("blob values not supported".into())))
        }
    }
}
