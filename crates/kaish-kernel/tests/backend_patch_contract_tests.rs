//! The batch contract on `KernelBackend::patch`, pinned against every
//! in-tree implementation.
//!
//! The trait doc states two promises that embedders build on: a failed batch
//! leaves the file untouched, and operations within one batch see each other's
//! edits. Both were true of `LocalBackend` and `VirtualOverlayBackend` before
//! the doc said so; these tests are what keep the doc from drifting away from
//! the code, and what a new backend can be checked against.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;
use std::sync::Arc;

use kaish_kernel::vfs::{Filesystem, MemoryFs, VfsRouter};
use kaish_kernel::{BackendError, KernelBackend, LocalBackend, PatchOp, VirtualOverlayBackend};

const ORIGINAL: &str = "alpha\nbravo\ncharlie\n";

/// Multi-byte content for the character-boundary cases: `é` occupies bytes
/// 0 and 1, so offset 1 is inside it.
const MULTIBYTE: &str = "école\nnaïve\n";

/// Every backend under test, with the path to exercise it on.
///
/// The overlay's path sits under a kaish mount so the operation stays in the
/// overlay rather than falling through to `inner` — the fall-through case is
/// `LocalBackend`'s row, already covered.
async fn backends() -> Vec<(&'static str, Arc<dyn KernelBackend>, &'static Path)> {
    backends_holding(ORIGINAL).await
}

/// `backends()` with the file's starting content chosen by the caller.
async fn backends_holding(content: &str) -> Vec<(&'static str, Arc<dyn KernelBackend>, &'static Path)> {
    let local = {
        let mem = MemoryFs::new();
        mem.write(Path::new("lines.txt"), content.as_bytes()).await.unwrap();
        let mut vfs = VfsRouter::new();
        vfs.mount("/", mem);
        Arc::new(LocalBackend::new(Arc::new(vfs))) as Arc<dyn KernelBackend>
    };

    let overlay = {
        // `inner` is never reached: the path under test sits on a kaish mount,
        // so the overlay handles it. An empty LocalBackend stands in for the
        // embedder's backend.
        let inner = Arc::new(LocalBackend::new(Arc::new(VfsRouter::new())));
        let blobs = MemoryFs::new();
        blobs.write(Path::new("lines.txt"), content.as_bytes()).await.unwrap();
        let mut vfs = VfsRouter::new();
        vfs.mount("/v/blobs", blobs);
        Arc::new(VirtualOverlayBackend::new(inner, Arc::new(vfs))) as Arc<dyn KernelBackend>
    };

    vec![
        ("LocalBackend", local, Path::new("/lines.txt")),
        ("VirtualOverlayBackend", overlay, Path::new("/v/blobs/lines.txt")),
    ]
}

async fn read_text(backend: &Arc<dyn KernelBackend>, path: &Path) -> String {
    let bytes = backend.read(path, None).await.unwrap();
    String::from_utf8(bytes).unwrap()
}

/// A batch is all-or-nothing: the first operation succeeds in memory, the
/// second fails its CAS check, and the file keeps every byte it had.
///
/// This is the test that would fail if an implementation wrote after each
/// operation. It would leave `ALPHA` on disk with an error returned — the
/// caller could not tell how far the batch got, which is the whole reason
/// the contract is worth stating.
#[tokio::test]
async fn a_failed_op_rolls_back_the_whole_batch() {
    for (name, backend, path) in backends().await {
        let ops = vec![
            PatchOp::ReplaceLine { line: 1, content: "ALPHA".to_string(), expected: None },
            PatchOp::ReplaceLine {
                line: 2,
                content: "BRAVO".to_string(),
                expected: Some("not what is there".to_string()),
            },
        ];

        let err = match backend.patch(path, &ops).await {
            Ok(()) => panic!("{name}: the second op's CAS must fail"),
            Err(e) => e,
        };
        assert!(
            matches!(err, BackendError::Conflict(_)),
            "{name}: expected a Conflict, got {err:?}",
        );

        assert_eq!(
            read_text(&backend, path).await,
            ORIGINAL,
            "{name}: a failed batch must leave the file untouched — \
             the first op's ALPHA must not survive",
        );
    }
}

/// Operations apply in order to one accumulating snapshot, so each sees the
/// edits before it. The insert shifts every later line down by one, and the
/// replace's line number and `expected` are read against the shifted content.
///
/// Under the other plausible reading — every op measured against the file as
/// it was on entry — line 2 would still be `bravo` and this CAS would fail.
/// That is what makes the assertion discriminating rather than decorative.
#[tokio::test]
async fn ops_within_a_batch_see_each_others_edits() {
    for (name, backend, path) in backends().await {
        let ops = vec![
            PatchOp::InsertLine { line: 1, content: "zero".to_string() },
            PatchOp::ReplaceLine {
                line: 2,
                content: "ONE".to_string(),
                expected: Some("alpha".to_string()),
            },
        ];

        backend.patch(path, &ops).await.unwrap_or_else(|e| {
            panic!("{name}: line 2 is `alpha` once the insert has applied: {e:?}")
        });

        assert_eq!(
            read_text(&backend, path).await,
            "zero\nONE\nbravo\ncharlie\n",
            "{name}: offsets and line numbers are relative to the accumulated content",
        );
    }
}

/// A byte offset inside a multi-byte character is refused, not a panic.
///
/// `insert_str`, `&content[a..b]`, `drain`, and `replace_range` all abort the
/// process on a mid-codepoint index. An embedder that computes its own byte
/// offset — off by one against a `é` — used to take the kernel down with it;
/// it gets an `InvalidOperation` naming the character instead.
#[tokio::test]
async fn a_mid_codepoint_offset_is_refused() {
    // Each case names the character its offset splits: `é` occupies bytes 0-1,
    // and `ï` bytes 9-10.
    let split_ops = [
        ("Insert", PatchOp::Insert { offset: 1, content: "x".to_string() }, 'é'),
        ("Delete", PatchOp::Delete { offset: 1, len: 1, expected: None }, 'é'),
        ("Delete end", PatchOp::Delete { offset: 0, len: 1, expected: None }, 'é'),
        (
            "Replace start",
            PatchOp::Replace { offset: 1, len: 1, content: "x".to_string(), expected: None },
            'é',
        ),
        // Starts on a boundary and ends inside `naïve`'s `ï` — the end check
        // is the one the other Replace case never reaches.
        (
            "Replace end",
            PatchOp::Replace { offset: 0, len: 10, content: "x".to_string(), expected: None },
            'ï',
        ),
    ];

    for (label, op, split) in split_ops {
        for (name, backend, path) in backends_holding(MULTIBYTE).await {
            let err = match backend.patch(path, std::slice::from_ref(&op)).await {
                Ok(()) => panic!("{name}/{label}: a mid-codepoint offset must be refused"),
                Err(e) => e,
            };
            assert!(
                matches!(err, BackendError::InvalidOperation(_)),
                "{name}/{label}: expected InvalidOperation, got {err:?}",
            );
            let message = err.to_string();
            assert!(
                message.contains("character boundary") && message.contains(split),
                "{name}/{label}: the error must name the character it splits ({split}): {message}",
            );
            assert_eq!(
                read_text(&backend, path).await,
                MULTIBYTE,
                "{name}/{label}: a refused op must leave the file untouched",
            );
        }
    }
}

/// `line: 0` is refused rather than mapped onto line 1.
///
/// `PatchOp`'s docs say line numbers are 1-indexed. `saturating_sub(1)` used
/// to turn a caller's 0-indexed line number into an edit one line off, which
/// is the silent-fallback shape: the wrong line is written and nobody is told.
#[tokio::test]
async fn line_zero_is_refused() {
    let zero_ops = [
        ("InsertLine", PatchOp::InsertLine { line: 0, content: "zero".to_string() }),
        ("DeleteLine", PatchOp::DeleteLine { line: 0, expected: None }),
        (
            "ReplaceLine",
            PatchOp::ReplaceLine { line: 0, content: "ZERO".to_string(), expected: None },
        ),
    ];

    for (label, op) in zero_ops {
        for (name, backend, path) in backends().await {
            let err = match backend.patch(path, std::slice::from_ref(&op)).await {
                Ok(()) => panic!("{name}/{label}: line 0 must be refused"),
                Err(e) => e,
            };
            assert!(
                matches!(err, BackendError::InvalidOperation(_)),
                "{name}/{label}: expected InvalidOperation, got {err:?}",
            );
            assert!(
                err.to_string().contains("1-indexed"),
                "{name}/{label}: the error must name the rule: {err}",
            );
            assert_eq!(
                read_text(&backend, path).await,
                ORIGINAL,
                "{name}/{label}: a refused op must leave the file untouched",
            );
        }
    }
}

/// A boundary that is a boundary still works.
///
/// The refusals above would pass just as well against a check that rejected
/// every multi-byte offset. This is the control: `école` is 6 bytes, `é`
/// takes the first two, and an edit at offset 2 lands between characters and
/// must apply.
#[tokio::test]
async fn a_valid_boundary_in_multibyte_content_still_applies() {
    for (name, backend, path) in backends_holding(MULTIBYTE).await {
        let ops = vec![PatchOp::Insert { offset: 2, content: "-".to_string() }];
        backend
            .patch(path, &ops)
            .await
            .unwrap_or_else(|e| panic!("{name}: offset 2 is a character boundary: {e:?}"));
        assert_eq!(
            read_text(&backend, path).await,
            "\u{e9}-cole\nna\u{ef}ve\n",
            "{name}: an insert on a boundary applies where it was asked to",
        );
    }
}
