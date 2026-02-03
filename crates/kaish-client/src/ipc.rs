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
use kaish_kernel::interpreter::{EntryType, ExecResult, OutputData, OutputNode};
use kaish_schema::{kernel, output_data, output_node};

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
        let socket_path = kaish_kernel::paths::runtime_dir().join("default.sock");
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
            output: if result.has_output() {
                Some(read_output_data(&result.get_output()?)?)
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
        let request = self.client.get_cwd_request();
        let response = request.send().promise.await?;
        let path = response.get()?.get_path()?.to_str()?;
        Ok(path.to_string())
    }

    async fn set_cwd(&self, path: &str) -> ClientResult<()> {
        let mut request = self.client.set_cwd_request();
        request.get().set_path(path);
        let response = request.send().promise.await?;
        let result = response.get()?;
        if !result.get_success() {
            let error = result.get_error()?.to_str()?;
            return Err(ClientError::Execution(error.to_string()));
        }
        Ok(())
    }

    async fn last_result(&self) -> ClientResult<ExecResult> {
        let request = self.client.get_last_result_request();
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
            output: if result.has_output() {
                Some(read_output_data(&result.get_output()?)?)
            } else {
                None
            },
        })
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

    async fn read_blob(&self, id: &str) -> ClientResult<Vec<u8>> {
        let mut request = self.client.read_blob_request();
        request.get().set_id(id);

        let response = request.send().promise.await?;
        let stream = response.get()?.get_stream()?;

        // Read all data from the stream
        let mut data = Vec::new();
        loop {
            let mut read_request = stream.read_request();
            read_request.get().set_max_bytes(64 * 1024); // 64KB chunks

            let read_response = read_request.send().promise.await?;
            let result = read_response.get()?;

            let chunk = result.get_data()?;
            data.extend_from_slice(chunk);

            if result.get_done() {
                break;
            }
        }

        Ok(data)
    }

    async fn write_blob(&self, content_type: &str, data: &[u8]) -> ClientResult<String> {
        let mut request = self.client.write_blob_request();
        {
            let mut params = request.get();
            params.set_content_type(content_type);
            params.set_size(data.len() as u64);
        }

        let response = request.send().promise.await?;
        let result = response.get()?;

        let id = result.get_id()?.to_str()?.to_string();
        let sink = result.get_stream()?;

        // Write data in chunks
        const CHUNK_SIZE: usize = 64 * 1024; // 64KB chunks
        for chunk in data.chunks(CHUNK_SIZE) {
            let mut write_request = sink.write_request();
            write_request.get().set_data(chunk);
            write_request.send().promise.await?;
        }

        // Finish the write
        let _finish_response = sink.finish_request().send().promise.await?;

        Ok(id)
    }

    async fn delete_blob(&self, id: &str) -> ClientResult<bool> {
        let mut request = self.client.delete_blob_request();
        request.get().set_id(id);

        let response = request.send().promise.await?;
        let success = response.get()?.get_success();

        Ok(success)
    }
}

// ============================================================
// Value Conversion Helpers
// ============================================================

use kaish_schema::value;

/// Convert a kaish Value to a Cap'n Proto Value.
fn set_value(builder: &mut value::Builder<'_>, value: &Value) {
    match value {
        Value::Null => builder.set_null(()),
        Value::Bool(b) => builder.set_bool(*b),
        Value::Int(i) => builder.set_int(*i),
        Value::Float(f) => builder.set_float(*f),
        Value::String(s) => builder.set_string(s),
        Value::Json(json) => {
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
                builder.set_string(&n.to_string());
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

/// Read a kaish Value from a Cap'n Proto Value.
///
/// Arrays and objects from the wire are converted to JSON strings.
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
        // Convert arrays to JSON string
        Which::Array(arr) => {
            let arr = arr?;
            let items: Result<Vec<serde_json::Value>, _> = arr
                .iter()
                .map(|v| read_value(&v).map(|v| value_to_json(&v)))
                .collect();
            let json = serde_json::Value::Array(items?);
            Ok(Value::String(json.to_string()))
        }
        // Convert objects to JSON string
        Which::Object(obj) => {
            let obj = obj?;
            let mut map = serde_json::Map::new();
            for kv in obj.iter() {
                let key_text = kv.get_key()?;
                let key = key_text.to_str().map_err(|e| {
                    ClientError::Rpc(capnp::Error::failed(format!("invalid utf8: {}", e)))
                })?.to_string();
                let val = read_value(&kv.get_value()?)?;
                map.insert(key, value_to_json(&val));
            }
            let json = serde_json::Value::Object(map);
            Ok(Value::String(json.to_string()))
        }
        Which::Blob(blob) => {
            // Convert blob reference to JSON string representation
            let blob = blob?;
            let id_text = blob.get_id()?;
            let id = id_text.to_str().map_err(|e| {
                ClientError::Rpc(capnp::Error::failed(format!("invalid utf8: {}", e)))
            })?;
            let size = blob.get_size();
            let content_type_text = blob.get_content_type()?;
            let content_type = content_type_text.to_str().map_err(|e| {
                ClientError::Rpc(capnp::Error::failed(format!("invalid utf8: {}", e)))
            })?;
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

/// Convert a kaish Value to serde_json::Value.
fn value_to_json(value: &Value) -> serde_json::Value {
    match value {
        Value::Null => serde_json::Value::Null,
        Value::Bool(b) => serde_json::Value::Bool(*b),
        Value::Int(i) => serde_json::Value::Number((*i).into()),
        Value::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        Value::String(s) => serde_json::Value::String(s.clone()),
        Value::Json(json) => json.clone(),
        Value::Blob(blob) => {
            let mut map = serde_json::Map::new();
            map.insert("_type".to_string(), serde_json::Value::String("blob".to_string()));
            map.insert("id".to_string(), serde_json::Value::String(blob.id.clone()));
            map.insert("size".to_string(), serde_json::Value::Number(blob.size.into()));
            map.insert("contentType".to_string(), serde_json::Value::String(blob.content_type.clone()));
            if let Some(hash) = &blob.hash {
                let hash_hex: String = hash.iter().map(|b| format!("{:02x}", b)).collect();
                map.insert("hash".to_string(), serde_json::Value::String(hash_hex));
            }
            serde_json::Value::Object(map)
        }
    }
}

// ============================================================
// OutputData Conversion Helpers
// ============================================================

/// Read an OutputData from a Cap'n Proto OutputData.
fn read_output_data(reader: &output_data::Reader<'_>) -> ClientResult<OutputData> {
    // Read headers if present
    let headers = if reader.has_headers() {
        let headers_reader = reader.get_headers()?;
        let mut headers = Vec::with_capacity(headers_reader.len() as usize);
        for header in headers_reader.iter() {
            headers.push(header?.to_str()?.to_string());
        }
        Some(headers)
    } else {
        None
    };

    // Read root nodes
    let root_reader = reader.get_root()?;
    let mut root = Vec::with_capacity(root_reader.len() as usize);
    for node_reader in root_reader.iter() {
        root.push(read_output_node(&node_reader)?);
    }

    Ok(OutputData { headers, root })
}

/// Read an OutputNode from a Cap'n Proto OutputNode (recursive).
fn read_output_node(reader: &output_node::Reader<'_>) -> ClientResult<OutputNode> {
    use kaish_schema::kaish_capnp::EntryType as SchemaEntryType;

    let name = reader.get_name()?.to_str()?.to_string();

    let entry_type = match reader.get_entry_type()? {
        SchemaEntryType::Text => EntryType::Text,
        SchemaEntryType::File => EntryType::File,
        SchemaEntryType::Directory => EntryType::Directory,
        SchemaEntryType::Executable => EntryType::Executable,
        SchemaEntryType::Symlink => EntryType::Symlink,
    };

    let text = if reader.has_text() {
        Some(reader.get_text()?.to_str()?.to_string())
    } else {
        None
    };

    let cells = if reader.has_cells() {
        let cells_reader = reader.get_cells()?;
        let mut cells = Vec::with_capacity(cells_reader.len() as usize);
        for cell in cells_reader.iter() {
            cells.push(cell?.to_str()?.to_string());
        }
        cells
    } else {
        Vec::new()
    };

    let children = if reader.has_children() {
        let children_reader = reader.get_children()?;
        let mut children = Vec::with_capacity(children_reader.len() as usize);
        for child_reader in children_reader.iter() {
            children.push(read_output_node(&child_reader)?);
        }
        children
    } else {
        Vec::new()
    };

    Ok(OutputNode {
        name,
        entry_type,
        text,
        cells,
        children,
    })
}
