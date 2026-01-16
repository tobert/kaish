//! Scheduler module for kaish — pipelines and background jobs.
//!
//! This module provides:
//! - **Pipeline execution**: Run commands connected by pipes, where stdout
//!   of one command flows to stdin of the next.
//! - **Background jobs**: Run commands in the background with `&`, track them,
//!   and wait for completion.
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
//! │                      JobManager                             │
//! │  jobs: HashMap<JobId, Job>                                  │
//! │  - spawn(pipeline) → JobId                                  │
//! │  - wait(JobId) → ExecResult                                 │
//! │  - wait_all() → Vec<ExecResult>                             │
//! │  - list() → Vec<JobInfo>                                    │
//! └─────────────────────────────────────────────────────────────┘
//! ```

mod job;
mod pipeline;

pub use job::{Job, JobId, JobInfo, JobManager, JobStatus};
pub use pipeline::PipelineRunner;
