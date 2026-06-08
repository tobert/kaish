//! Scheduler module for kaish — pipelines, background jobs, and scatter/gather.
//!
//! This module provides:
//! - **Pipeline execution**: Run commands connected by pipes, where stdout
//!   of one command flows to stdin of the next.
//! - **Background jobs**: Run commands in the background with `&`, track them,
//!   and wait for completion.
//! - **Scatter/Gather**: Parallel fan-out and collection for pipelines.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                     PipelineRunner                          │
//! │  ┌─────────┐   channel   ┌─────────┐   channel   ┌────────┐│
//! │  │ cmd1    │────────────▶│ cmd2    │────────────▶│ cmd3   ││
//! │  │ (spawn) │   stdout    │ (spawn) │   stdout    │ (spawn)││
//! │  └─────────┘             └─────────┘             └────────┘│
//! └─────────────────────────────────────────────────────────────┘
//!
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    ScatterGatherRunner                        │
//! │  ┌────────────┐         ┌─────────────────────────┐        │
//! │  │ pre_scatter│────────▶│  scatter (fan-out)      │        │
//! │  └────────────┘         │  ┌───┐ ┌───┐ ┌───┐      │        │
//! │                         │  │ 1 │ │ 2 │ │ 3 │ ...  │        │
//! │                         │  └───┘ └───┘ └───┘      │        │
//! │                         │         │               │        │
//! │                         │         ▼               │        │
//! │                         │  gather (collect)       │        │
//! │                         └─────────────────────────┘        │
//! │                                    │                       │
//! │                                    ▼                       │
//! │                         ┌────────────┐                     │
//! │                         │ post_gather│                     │
//! │                         └────────────┘                     │
//! └─────────────────────────────────────────────────────────────┘
//!
//! ┌─────────────────────────────────────────────────────────────┐
//! │                      JobManager                             │
//! │  jobs: HashMap<JobId, Job>                                  │
//! │  - spawn(pipeline) → JobId                                  │
//! │  - wait(JobId) → ExecResult                                 │
//! │  - wait_all() → Vec<ExecResult>                             │
//! │  - list() → Vec<JobInfo>                                    │
//! └─────────────────────────────────────────────────────────────┘
//! ```

mod job;
mod pipe_stream;
pub(crate) mod pipeline;
mod scatter;
mod stderr_stream;
mod stream;

pub use job::{Job, JobId, JobInfo, JobManager, JobStatus};
pub use pipe_stream::{pipe_stream, pipe_stream_default, PipeReader, PipeWriter, PIPE_BUFFER_SIZE};
pub use stderr_stream::{stderr_stream, StderrReceiver, StderrStream};
pub use stream::{drain_to_stream, BoundedStream, StreamStats, DEFAULT_STREAM_MAX_SIZE};
pub use pipeline::{build_tool_args, is_bool_type, schema_param_lookup, select_leaf, PipelineRunner};
pub use scatter::{
    extract_items, parse_gather_options, parse_scatter_options, GatherOptions,
    ScatterGatherRunner, ScatterOptions, ScatterResult,
};
