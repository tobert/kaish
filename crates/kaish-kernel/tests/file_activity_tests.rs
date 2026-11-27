//! Integration tests for the file-activity pass:
//! - Typed embedder invocation identity carried across foreground, pipeline,
//!   scatter/gather, and `&` background jobs (with NO tracing subscriber).
//! - `&` background job lifetime exposure (`ExecResult::background_jobs`,
//!   `JobManager::wait_jobs`, `JobManager::invocation`, `JobInfo::finished_at`).
//! - Explicit content reads vs edit/script/internal reads via
//!   `KernelBackend::read_with_purpose` and `ReadPurpose`.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use async_trait::async_trait;
use kaish_kernel::backend::{
    BackendResult, KernelBackend, LocalBackend, PatchOp, ReadPurpose, ReadRange, ToolInfo,
    ToolResult, WriteMode,
};
use kaish_kernel::tools::ToolArgs;
use kaish_kernel::trash::{TrashBackend, TrashEntry, TrashError};
use kaish_kernel::vfs::{DirEntry, Filesystem, MemoryFs, VfsRouter};
use kaish_kernel::{current_invocation, ExecuteOptions, Kernel, KernelConfig};
use kaish_tool_api::ToolCtx;
use kaish_types::backend::MountInfo;
use kaish_types::PathAccess;

#[derive(Debug, Clone, PartialEq, Eq)]
struct InvocationId(&'static str);

#[derive(Debug, Clone, PartialEq, Eq)]
struct RecordedOp {
    kind: &'static str,
    path: PathBuf,
    purpose: Option<ReadPurpose>,
    invocation: Option<InvocationId>,
}

struct RecordingBackend {
    inner: LocalBackend,
    ops: Arc<Mutex<Vec<RecordedOp>>>,
}

impl RecordingBackend {
    fn new(vfs: Arc<VfsRouter>) -> (Self, Arc<Mutex<Vec<RecordedOp>>>) {
        let ops = Arc::new(Mutex::new(Vec::new()));
        (
            Self {
                inner: LocalBackend::new(vfs),
                ops: ops.clone(),
            },
            ops,
        )
    }

    fn record(&self, kind: &'static str, path: &Path, purpose: Option<ReadPurpose>) {
        let invocation = current_invocation::<InvocationId>().map(|arc| (*arc).clone());
        self.ops.lock().unwrap().push(RecordedOp {
            kind,
            path: path.to_path_buf(),
            purpose,
            invocation,
        });
    }
}

#[async_trait]
impl KernelBackend for RecordingBackend {
    async fn read(&self, path: &Path, range: Option<ReadRange>) -> BackendResult<Vec<u8>> {
        self.record("read", path, None);
        self.inner.read(path, range).await
    }

    async fn read_with_purpose(
        &self,
        path: &Path,
        range: Option<ReadRange>,
        purpose: ReadPurpose,
    ) -> BackendResult<Vec<u8>> {
        self.record("read_with_purpose", path, Some(purpose));
        self.inner.read_with_purpose(path, range, purpose).await
    }

    async fn write(&self, path: &Path, content: &[u8], mode: WriteMode) -> BackendResult<()> {
        self.record("write", path, None);
        self.inner.write(path, content, mode).await
    }

    async fn append(&self, path: &Path, content: &[u8]) -> BackendResult<()> {
        self.record("append", path, None);
        self.inner.append(path, content).await
    }

    async fn patch(&self, path: &Path, ops: &[PatchOp]) -> BackendResult<()> {
        self.record("patch", path, None);
        self.inner.patch(path, ops).await
    }

    async fn list(&self, path: &Path) -> BackendResult<Vec<DirEntry>> {
        self.inner.list(path).await
    }

    async fn stat(&self, path: &Path) -> BackendResult<DirEntry> {
        self.inner.stat(path).await
    }

    async fn mkdir(&self, path: &Path) -> BackendResult<()> {
        self.record("mkdir", path, None);
        self.inner.mkdir(path).await
    }

    async fn set_mtime(&self, path: &Path, mtime: std::time::SystemTime) -> BackendResult<()> {
        self.inner.set_mtime(path, mtime).await
    }

    async fn remove(&self, path: &Path, recursive: bool) -> BackendResult<()> {
        self.record("remove", path, None);
        self.inner.remove(path, recursive).await
    }

    async fn rename(&self, from: &Path, to: &Path) -> BackendResult<()> {
        self.record("rename", to, None);
        self.inner.rename(from, to).await
    }

    async fn exists(&self, path: &Path) -> bool {
        self.inner.exists(path).await
    }

    async fn lstat(&self, path: &Path) -> BackendResult<DirEntry> {
        self.inner.lstat(path).await
    }

    async fn read_link(&self, path: &Path) -> BackendResult<PathBuf> {
        self.inner.read_link(path).await
    }

    async fn symlink(&self, target: &Path, link: &Path) -> BackendResult<()> {
        self.inner.symlink(target, link).await
    }

    async fn path_access(&self, path: &Path) -> BackendResult<PathAccess> {
        self.inner.path_access(path).await
    }

    async fn call_tool(
        &self,
        name: &str,
        args: ToolArgs,
        ctx: &mut dyn ToolCtx,
    ) -> BackendResult<ToolResult> {
        self.inner.call_tool(name, args, ctx).await
    }

    async fn list_tools(&self) -> BackendResult<Vec<ToolInfo>> {
        self.inner.list_tools().await
    }

    async fn get_tool(&self, name: &str) -> BackendResult<Option<ToolInfo>> {
        self.inner.get_tool(name).await
    }

    fn mounts(&self) -> Vec<MountInfo> {
        self.inner.mounts()
    }

    fn backend_type(&self) -> &str {
        "recording"
    }

    fn resolve_real_path(&self, path: &Path) -> Option<PathBuf> {
        self.inner.resolve_real_path(path)
    }

    fn read_only(&self) -> bool {
        self.inner.read_only()
    }
}

struct NoopTrash;

#[async_trait]
impl TrashBackend for NoopTrash {
    async fn trash(&self, _path: &Path) -> Result<(), TrashError> {
        Ok(())
    }

    async fn trash_bytes(&self, _original_path: &Path, _bytes: &[u8]) -> Result<(), TrashError> {
        Ok(())
    }

    async fn list(&self, _filter: Option<&str>) -> Result<Vec<TrashEntry>, TrashError> {
        Ok(Vec::new())
    }

    async fn find_by_name(&self, _name: &str) -> Result<Vec<TrashEntry>, TrashError> {
        Ok(Vec::new())
    }

    async fn restore(&self, _entries: Vec<TrashEntry>) -> Result<(), TrashError> {
        Ok(())
    }

    async fn purge_all(&self) -> Result<usize, TrashError> {
        Ok(0)
    }
}

fn make_kernel() -> (Kernel, Arc<VfsRouter>, Arc<Mutex<Vec<RecordedOp>>>) {
    let mut router = VfsRouter::new();
    router.mount("/", MemoryFs::new());
    let vfs = Arc::new(router);
    let (backend, ops) = RecordingBackend::new(vfs.clone());
    let mut kernel = Kernel::with_backend(
        Arc::new(backend),
        KernelConfig::isolated().with_cwd(PathBuf::from("/")),
        |_| {},
        |_| {},
    )
    .expect("kernel creation should succeed");
    kernel.set_trash_backend(Some(Arc::new(NoopTrash)));
    (kernel, vfs, ops)
}

#[tokio::test]
async fn invocation_propagates_across_foreground_pipeline_and_scatter_without_tracing() {
    let (kernel, vfs, ops) = make_kernel();
    vfs.write(Path::new("/a.txt"), b"alpha\nbeta\n").await.unwrap();
    vfs.write(Path::new("/item_one.txt"), b"1\n").await.unwrap();
    vfs.write(Path::new("/item_two.txt"), b"2\n").await.unwrap();

    // 1. Foreground read + write
    let r = kernel
        .execute_with_options(
            "cat /a.txt > /fg_out.txt",
            ExecuteOptions::new().with_invocation(InvocationId("inv-fg")),
        )
        .await
        .unwrap();
    assert!(r.ok(), "err: {}", r.err);
    assert!(r.background_jobs.is_empty());

    // 2. Multi-stage concurrent pipeline (stage 0 reads, stage 2 writes via tee)
    let r = kernel
        .execute_with_options(
            "cat /a.txt | grep alpha | tee /pipe_out.txt",
            ExecuteOptions::new().with_invocation(InvocationId("inv-pipe")),
        )
        .await
        .unwrap();
    assert!(r.ok(), "err: {}", r.err);

    // 3. Scatter/gather workers
    let r = kernel
        .execute_with_options(
            r#"printf 'one\ntwo\n' | scatter as=ITEM | cat "/item_${ITEM}.txt" | gather"#,
            ExecuteOptions::new().with_invocation(InvocationId("inv-scatter")),
        )
        .await
        .unwrap();
    assert!(r.ok(), "err: {}", r.err);

    // 4. Call with no invocation must observe None (no stale bleed)
    let r = kernel
        .execute_with_options("cat /a.txt", ExecuteOptions::new())
        .await
        .unwrap();
    assert!(r.ok(), "err: {}", r.err);

    let recorded = ops.lock().unwrap().clone();
    assert!(
        recorded.iter().any(|op| op.path == Path::new("/fg_out.txt")
            && op.kind == "write"
            && op.invocation == Some(InvocationId("inv-fg"))),
        "foreground write must carry inv-fg: {recorded:?}"
    );
    assert!(
        recorded.iter().any(|op| op.path == Path::new("/a.txt")
            && op.invocation == Some(InvocationId("inv-pipe"))),
        "pipeline stage 0 read must carry inv-pipe: {recorded:?}"
    );
    assert!(
        recorded.iter().any(|op| op.path == Path::new("/pipe_out.txt")
            && op.kind == "write"
            && op.invocation == Some(InvocationId("inv-pipe"))),
        "pipeline last stage write must carry inv-pipe: {recorded:?}"
    );
    assert!(
        recorded.iter().any(|op| op.path == Path::new("/item_one.txt")
            && op.invocation == Some(InvocationId("inv-scatter"))),
        "scatter worker 1 read must carry inv-scatter: {recorded:?}"
    );
    assert!(
        recorded.iter().any(|op| op.path == Path::new("/item_two.txt")
            && op.invocation == Some(InvocationId("inv-scatter"))),
        "scatter worker 2 read must carry inv-scatter: {recorded:?}"
    );
    assert!(
        recorded
            .iter()
            .any(|op| op.path == Path::new("/a.txt") && op.invocation.is_none()),
        "call without invocation must see None: {recorded:?}"
    );
}

#[tokio::test]
async fn background_jobs_expose_lifetime_and_attribute_delayed_writes() {
    let (kernel, _vfs, ops) = make_kernel();

    // `&` jobs spawned at top level, inside `if`, inside `$(…)`, and inside a
    // non-last pipeline stage are all captured in `ExecResult::background_jobs`.
    let script = r#"
        { sleep 0.03; echo first > /bg1.txt; } &
        if true; then
            { sleep 0.05; echo second > /bg2.txt; } &
        fi
        CAPTURED=$({ sleep 0.02; echo sub > /bg3.txt; } &)
        { { sleep 0.02; echo stage > /bg4.txt; } & } | cat
    "#;

    let res = kernel
        .execute_with_options(
            script,
            ExecuteOptions::new().with_invocation(InvocationId("inv-bg")),
        )
        .await
        .unwrap();
    assert!(res.ok(), "err: {}", res.err);
    assert_eq!(
        res.background_jobs.len(),
        4,
        "ExecResult::background_jobs should list all 4 spawned `&` jobs: {:?}",
        res.background_jobs
    );

    // Each job records its originating invocation identity on JobManager.
    for &id in &res.background_jobs {
        let inv = kernel.jobs().invocation::<InvocationId>(id).await;
        assert_eq!(inv.as_deref(), Some(&InvocationId("inv-bg")));
        // Wrong concrete type returns None.
        assert!(kernel.jobs().invocation::<u64>(id).await.is_none());
    }

    // Await just the jobs spawned by this execution.
    let finished = kernel.jobs().wait_jobs(&res.background_jobs).await;
    assert_eq!(finished.len(), 4);
    for (id, job_res) in &finished {
        assert!(job_res.ok(), "job {id} failed: {}", job_res.err);
        let info = kernel.jobs().get(*id).await.unwrap();
        assert!(
            info.finished_at.is_some(),
            "finished job {id} should carry finished_at"
        );
    }

    // Delayed writes performed after `execute_with_options` returned still
    // carry `inv-bg`.
    let recorded = ops.lock().unwrap().clone();
    for target in ["/bg1.txt", "/bg2.txt", "/bg3.txt", "/bg4.txt"] {
        assert!(
            recorded.iter().any(|op| op.path == Path::new(target)
                && op.kind == "write"
                && op.invocation == Some(InvocationId("inv-bg"))),
            "delayed write to {target} must carry inv-bg: {recorded:?}"
        );
    }
}

#[tokio::test]
async fn execute_background_with_options_propagates_invocation() {
    let (kernel, _vfs, ops) = make_kernel();

    let job_id = kernel
        .execute_background_with_options(
            "echo whole > /whole_bg.txt",
            ExecuteOptions::new().with_invocation(InvocationId("inv-whole")),
        )
        .await
        .unwrap();

    assert_eq!(
        kernel
            .jobs()
            .invocation::<InvocationId>(job_id)
            .await
            .as_deref(),
        Some(&InvocationId("inv-whole"))
    );

    let result = kernel.jobs().wait(job_id).await.unwrap();
    assert!(result.ok(), "err: {}", result.err);

    let recorded = ops.lock().unwrap().clone();
    assert!(
        recorded.iter().any(|op| op.path == Path::new("/whole_bg.txt")
            && op.kind == "write"
            && op.invocation == Some(InvocationId("inv-whole"))),
        "whole-program background write must carry inv-whole: {recorded:?}"
    );

    // Calling `execute_background_with_options` with `ExecuteOptions::new()`
    // (no invocation) must stamp `None` on both `JobManager` and backend calls,
    // never leaking an enclosing invocation.
    let job_none = kernel
        .execute_background_with_options(
            "echo plain > /whole_none.txt",
            ExecuteOptions::new(),
        )
        .await
        .unwrap();
    assert!(kernel.jobs().invocation::<InvocationId>(job_none).await.is_none());
    let res_none = kernel.jobs().wait(job_none).await.unwrap();
    assert!(res_none.ok(), "err: {}", res_none.err);
    let recorded = ops.lock().unwrap().clone();
    assert!(
        recorded.iter().any(|op| op.path == Path::new("/whole_none.txt")
            && op.kind == "write"
            && op.invocation.is_none()),
        "whole-program background write without invocation must see None: {recorded:?}"
    );
}

#[tokio::test]
async fn notify_finished_stamps_finished_at_without_wait() {
    let (kernel, _vfs, _ops) = make_kernel();
    let res = kernel.execute("{ echo done > /fast.txt; } &").await.unwrap();
    assert_eq!(res.background_jobs.len(), 1);
    let job_id = res.background_jobs[0];

    // Give the fast background job a moment to finish; `send_job_result` calls
    // `notify_finished`, which stamps `finished_at` without requiring `wait()`.
    for _ in 0..50 {
        let info = kernel.jobs().get(job_id).await.unwrap();
        if info.finished_at.is_some() {
            return;
        }
        tokio::time::sleep(std::time::Duration::from_millis(5)).await;
    }
    let info = kernel.jobs().get(job_id).await.unwrap();
    panic!("expected finished_at to be stamped promptly, got: {info:?}");
}

#[tokio::test]
async fn read_with_purpose_distinguishes_content_edit_script_and_internal_reads() {
    let (kernel, vfs, ops) = make_kernel();
    vfs.write(Path::new("/doc.txt"), b"hello world\nsecond line\n")
        .await
        .unwrap();
    vfs.write(Path::new("/lib.kai"), b"LIB_VAR=loaded\n")
        .await
        .unwrap();
    vfs.write(Path::new("/edit_me.txt"), b"alpha\nbeta\n")
        .await
        .unwrap();
    vfs.mkdir(Path::new("/sub/.git")).await.unwrap();
    vfs.write(Path::new("/sub/.gitignore"), b"*.ignored\n")
        .await
        .unwrap();
    vfs.write(Path::new("/sub/keep.txt"), b"needle\n")
        .await
        .unwrap();

    // 1. Explicit content reads: cat, head, grep, wc, file, and `<` redirect
    for cmd in [
        "cat /doc.txt",
        "head -n 1 /doc.txt",
        "grep hello /doc.txt",
        "wc -l /doc.txt",
        "file /doc.txt",
        "cat < /doc.txt",
    ] {
        ops.lock().unwrap().clear();
        let r = kernel.execute(cmd).await.unwrap();
        assert!(r.ok(), "{cmd} failed: {}", r.err);
        let recorded = ops.lock().unwrap().clone();
        assert!(
            recorded.iter().any(|op| op.path == Path::new("/doc.txt")
                && op.purpose == Some(ReadPurpose::Content)),
            "{cmd} must issue ReadPurpose::Content for /doc.txt: {recorded:?}"
        );
    }

    // 2. Edit / mutation-prep reads: cp and sed -i
    ops.lock().unwrap().clear();
    let r = kernel.execute("cp /edit_me.txt /copied.txt").await.unwrap();
    assert!(r.ok(), "cp failed: {}", r.err);
    let recorded = ops.lock().unwrap().clone();
    assert!(
        recorded.iter().any(|op| op.path == Path::new("/edit_me.txt")
            && op.purpose == Some(ReadPurpose::Edit)),
        "cp must issue ReadPurpose::Edit: {recorded:?}"
    );

    ops.lock().unwrap().clear();
    let r = kernel
        .execute("sed -i 's/alpha/ALPHA/' /edit_me.txt")
        .await
        .unwrap();
    assert!(r.ok(), "sed -i failed: {}", r.err);
    let recorded = ops.lock().unwrap().clone();
    assert!(
        recorded.iter().any(|op| op.path == Path::new("/edit_me.txt")
            && op.purpose == Some(ReadPurpose::Edit)),
        "sed -i must issue ReadPurpose::Edit: {recorded:?}"
    );

    // 3. Script read: source
    ops.lock().unwrap().clear();
    let r = kernel.execute("source /lib.kai").await.unwrap();
    assert!(r.ok(), "source failed: {}", r.err);
    let recorded = ops.lock().unwrap().clone();
    assert!(
        recorded.iter().any(|op| op.path == Path::new("/lib.kai")
            && op.purpose == Some(ReadPurpose::Script)),
        "source must issue ReadPurpose::Script: {recorded:?}"
    );

    // 4. Internal read: .gitignore during `grep -r`
    ops.lock().unwrap().clear();
    let r = kernel
        .execute("kaish-ignore add .gitignore; grep -r needle /sub")
        .await
        .unwrap();
    assert!(r.ok(), "grep -r failed: {}", r.err);
    let recorded = ops.lock().unwrap().clone();
    assert!(
        recorded
            .iter()
            .any(|op| op.path == Path::new("/sub/.gitignore")
                && op.purpose == Some(ReadPurpose::Internal)),
        "directory walk .gitignore read must be ReadPurpose::Internal: {recorded:?}"
    );
    assert!(
        recorded
            .iter()
            .any(|op| op.path == Path::new("/sub/keep.txt")
                && op.purpose == Some(ReadPurpose::Content)),
        "grep -r content file read must be ReadPurpose::Content: {recorded:?}"
    );

    // 5. Internal read: trash snapshot + CAS re-read under `set -o trash`
    ops.lock().unwrap().clear();
    let r = kernel
        .execute("set -o trash; echo replaced | tee /edit_me.txt")
        .await
        .unwrap();
    assert!(r.ok(), "tee under trash failed: {}", r.err);
    let recorded = ops.lock().unwrap().clone();
    assert!(
        recorded
            .iter()
            .any(|op| op.path == Path::new("/edit_me.txt")
                && op.purpose == Some(ReadPurpose::Internal)),
        "trash snapshot/CAS read must be ReadPurpose::Internal: {recorded:?}"
    );
    assert!(
        !recorded
            .iter()
            .any(|op| op.path == Path::new("/edit_me.txt")
                && op.purpose == Some(ReadPurpose::Content)),
        "trash snapshot/CAS must never report ReadPurpose::Content: {recorded:?}"
    );
}
