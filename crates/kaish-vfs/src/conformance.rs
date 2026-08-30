//! Cross-backend symlink conformance cases.
//!
//! Each case gets a fresh, empty writable root and exercises one symlink
//! behavior against the [`Filesystem`] trait. An embedder runs the whole
//! suite against its own backend via [`run_all`], supplying an adapter
//! that builds a fresh root per case.

use crate::Filesystem;
use std::future::Future;
use std::path::Path;
use std::pin::Pin;

/// One conformance case: an empty writable root in, pass/fail out.
pub type Case = for<'a> fn(
    &'a dyn Filesystem,
) -> Pin<Box<dyn Future<Output = Result<(), String>> + Send + 'a>>;

pub async fn lstat_reports_the_link_itself(fs: &dyn Filesystem) -> Result<(), String> {
    fs.write(Path::new("target"), b"TARGET")
        .await
        .map_err(|e| format!("write target: {e}"))?;
    fs.symlink(Path::new("target"), Path::new("link"))
        .await
        .map_err(|e| format!("symlink: {e}"))?;

    let lstat_entry = fs
        .lstat(Path::new("link"))
        .await
        .map_err(|e| format!("lstat(link): {e}"))?;
    if !lstat_entry.is_symlink() {
        return Err(format!(
            "expected lstat(link) to report symlink kind, got {:?}",
            lstat_entry.kind
        ));
    }

    let stat_entry = fs
        .stat(Path::new("link"))
        .await
        .map_err(|e| format!("stat(link): {e}"))?;
    if !stat_entry.is_file() {
        return Err(format!(
            "expected stat(link) to report a regular file, got {:?}",
            stat_entry.kind
        ));
    }
    Ok(())
}

pub async fn read_link_returns_the_target_verbatim(fs: &dyn Filesystem) -> Result<(), String> {
    fs.symlink(Path::new("target"), Path::new("link"))
        .await
        .map_err(|e| format!("symlink: {e}"))?;

    let target = fs
        .read_link(Path::new("link"))
        .await
        .map_err(|e| format!("read_link(link): {e}"))?;
    if target != Path::new("target") {
        return Err(format!(
            "expected read_link(link) == \"target\", got {}",
            target.display()
        ));
    }
    Ok(())
}

pub async fn relative_target_resolves_from_the_link_directory(
    fs: &dyn Filesystem,
) -> Result<(), String> {
    fs.write(Path::new("target"), b"ROOT")
        .await
        .map_err(|e| format!("write root target: {e}"))?;
    fs.mkdir(Path::new("d"))
        .await
        .map_err(|e| format!("mkdir d: {e}"))?;
    fs.write(Path::new("d/target"), b"D")
        .await
        .map_err(|e| format!("write d/target: {e}"))?;
    fs.symlink(Path::new("target"), Path::new("d/link"))
        .await
        .map_err(|e| format!("symlink d/link -> target: {e}"))?;

    let data = fs
        .read(Path::new("d/link"))
        .await
        .map_err(|e| format!("read(d/link): {e}"))?;
    if data != b"D" {
        return Err(format!(
            "expected read(d/link) == b\"D\" (resolved from d/), got {:?}",
            String::from_utf8_lossy(&data)
        ));
    }

    let entry = fs
        .stat(Path::new("d/link"))
        .await
        .map_err(|e| format!("stat(d/link): {e}"))?;
    if !entry.is_file() {
        return Err(format!(
            "expected stat(d/link) to be a regular file, got {:?}",
            entry.kind
        ));
    }
    Ok(())
}

pub async fn dangling_link_is_visible_to_lstat_but_not_stat(
    fs: &dyn Filesystem,
) -> Result<(), String> {
    fs.symlink(Path::new("nowhere"), Path::new("link"))
        .await
        .map_err(|e| format!("symlink: {e}"))?;

    let lstat_entry = fs
        .lstat(Path::new("link"))
        .await
        .map_err(|e| format!("lstat(link): {e}"))?;
    if !lstat_entry.is_symlink() {
        return Err(format!(
            "expected lstat(link) to report symlink kind, got {:?}",
            lstat_entry.kind
        ));
    }

    match fs.stat(Path::new("link")).await {
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => {
            return Err(format!(
                "expected stat(link) to be Err(NotFound), got a different error: {e}"
            ))
        }
        Ok(entry) => {
            return Err(format!(
                "expected stat(link) to be Err(NotFound), got Ok({:?})",
                entry.kind
            ))
        }
    }

    if fs.exists(Path::new("link")).await {
        return Err("expected exists(link) == false for a dangling link".to_string());
    }

    let target = fs
        .read_link(Path::new("link"))
        .await
        .map_err(|e| format!("read_link(link): {e}"))?;
    if target != Path::new("nowhere") {
        return Err(format!(
            "expected read_link(link) == \"nowhere\", got {}",
            target.display()
        ));
    }
    Ok(())
}

pub async fn remove_unlinks_the_link_and_keeps_the_file(
    fs: &dyn Filesystem,
) -> Result<(), String> {
    fs.write(Path::new("target"), b"TARGET")
        .await
        .map_err(|e| format!("write target: {e}"))?;
    fs.symlink(Path::new("target"), Path::new("link"))
        .await
        .map_err(|e| format!("symlink: {e}"))?;
    fs.remove(Path::new("link"))
        .await
        .map_err(|e| format!("remove(link): {e}"))?;

    if let Ok(entry) = fs.lstat(Path::new("link")).await {
        return Err(format!(
            "expected lstat(link) to be Err after remove, got Ok({:?})",
            entry.kind
        ));
    }

    let data = fs
        .read(Path::new("target"))
        .await
        .map_err(|e| format!("read(target): {e}"))?;
    if data != b"TARGET" {
        return Err(format!(
            "expected read(target) == b\"TARGET\" after removing the link, got {:?}",
            String::from_utf8_lossy(&data)
        ));
    }
    Ok(())
}

pub async fn remove_unlinks_a_link_to_a_directory(fs: &dyn Filesystem) -> Result<(), String> {
    fs.mkdir(Path::new("dir"))
        .await
        .map_err(|e| format!("mkdir dir: {e}"))?;
    fs.write(Path::new("dir/inner"), b"INNER")
        .await
        .map_err(|e| format!("write dir/inner: {e}"))?;
    fs.symlink(Path::new("dir"), Path::new("link"))
        .await
        .map_err(|e| format!("symlink: {e}"))?;
    fs.remove(Path::new("link"))
        .await
        .map_err(|e| format!("remove(link): {e}"))?;

    if let Ok(entry) = fs.lstat(Path::new("link")).await {
        return Err(format!(
            "expected lstat(link) to be Err after remove, got Ok({:?})",
            entry.kind
        ));
    }

    let dir_entry = fs
        .stat(Path::new("dir"))
        .await
        .map_err(|e| format!("stat(dir): {e}"))?;
    if !dir_entry.is_dir() {
        return Err(format!(
            "expected dir to still be a directory, got {:?}",
            dir_entry.kind
        ));
    }

    let data = fs
        .read(Path::new("dir/inner"))
        .await
        .map_err(|e| format!("read(dir/inner): {e}"))?;
    if data != b"INNER" {
        return Err(format!(
            "expected read(dir/inner) == b\"INNER\", got {:?}",
            String::from_utf8_lossy(&data)
        ));
    }
    Ok(())
}

pub async fn rename_moves_the_link_not_the_target(fs: &dyn Filesystem) -> Result<(), String> {
    fs.write(Path::new("target"), b"TARGET")
        .await
        .map_err(|e| format!("write target: {e}"))?;
    fs.symlink(Path::new("target"), Path::new("link"))
        .await
        .map_err(|e| format!("symlink: {e}"))?;
    fs.rename(Path::new("link"), Path::new("link2"))
        .await
        .map_err(|e| format!("rename(link, link2): {e}"))?;

    if let Ok(entry) = fs.lstat(Path::new("link")).await {
        return Err(format!(
            "expected lstat(link) to be Err after rename, got Ok({:?})",
            entry.kind
        ));
    }

    let link2_entry = fs
        .lstat(Path::new("link2"))
        .await
        .map_err(|e| format!("lstat(link2): {e}"))?;
    if !link2_entry.is_symlink() {
        return Err(format!(
            "expected lstat(link2) to report symlink kind, got {:?}",
            link2_entry.kind
        ));
    }

    let target = fs
        .read_link(Path::new("link2"))
        .await
        .map_err(|e| format!("read_link(link2): {e}"))?;
    if target != Path::new("target") {
        return Err(format!(
            "expected read_link(link2) == \"target\", got {}",
            target.display()
        ));
    }

    let target_data = fs
        .read(Path::new("target"))
        .await
        .map_err(|e| format!("read(target): {e}"))?;
    if target_data != b"TARGET" {
        return Err(format!(
            "expected read(target) == b\"TARGET\", got {:?}",
            String::from_utf8_lossy(&target_data)
        ));
    }

    let link2_data = fs
        .read(Path::new("link2"))
        .await
        .map_err(|e| format!("read(link2): {e}"))?;
    if link2_data != b"TARGET" {
        return Err(format!(
            "expected read(link2) == b\"TARGET\", got {:?}",
            String::from_utf8_lossy(&link2_data)
        ));
    }
    Ok(())
}

pub async fn rename_onto_a_file_link_replaces_the_link(
    fs: &dyn Filesystem,
) -> Result<(), String> {
    fs.write(Path::new("src"), b"NEW")
        .await
        .map_err(|e| format!("write src: {e}"))?;
    fs.write(Path::new("target"), b"TARGET")
        .await
        .map_err(|e| format!("write target: {e}"))?;
    fs.symlink(Path::new("target"), Path::new("link"))
        .await
        .map_err(|e| format!("symlink: {e}"))?;
    fs.rename(Path::new("src"), Path::new("link"))
        .await
        .map_err(|e| format!("rename(src, link): {e}"))?;

    let link_entry = fs
        .lstat(Path::new("link"))
        .await
        .map_err(|e| format!("lstat(link): {e}"))?;
    if !link_entry.is_file() {
        return Err(format!(
            "expected lstat(link) to be a regular file after rename onto it, got {:?}",
            link_entry.kind
        ));
    }

    let link_data = fs
        .read(Path::new("link"))
        .await
        .map_err(|e| format!("read(link): {e}"))?;
    if link_data != b"NEW" {
        return Err(format!(
            "expected read(link) == b\"NEW\", got {:?}",
            String::from_utf8_lossy(&link_data)
        ));
    }

    let target_data = fs
        .read(Path::new("target"))
        .await
        .map_err(|e| format!("read(target): {e}"))?;
    if target_data != b"TARGET" {
        return Err(format!(
            "expected read(target) unchanged == b\"TARGET\" (rename must not write through the link), got {:?}",
            String::from_utf8_lossy(&target_data)
        ));
    }

    if let Ok(entry) = fs.lstat(Path::new("src")).await {
        return Err(format!(
            "expected lstat(src) to be Err after rename, got Ok({:?})",
            entry.kind
        ));
    }
    Ok(())
}

pub async fn rename_onto_a_dangling_link_replaces_the_link(
    fs: &dyn Filesystem,
) -> Result<(), String> {
    fs.write(Path::new("src"), b"NEW")
        .await
        .map_err(|e| format!("write src: {e}"))?;
    fs.symlink(Path::new("nowhere"), Path::new("link"))
        .await
        .map_err(|e| format!("symlink: {e}"))?;
    fs.rename(Path::new("src"), Path::new("link"))
        .await
        .map_err(|e| format!("rename(src, link): {e}"))?;

    let link_entry = fs
        .lstat(Path::new("link"))
        .await
        .map_err(|e| format!("lstat(link): {e}"))?;
    if !link_entry.is_file() {
        return Err(format!(
            "expected lstat(link) to be a regular file after rename onto it, got {:?}",
            link_entry.kind
        ));
    }

    let link_data = fs
        .read(Path::new("link"))
        .await
        .map_err(|e| format!("read(link): {e}"))?;
    if link_data != b"NEW" {
        return Err(format!(
            "expected read(link) == b\"NEW\", got {:?}",
            String::from_utf8_lossy(&link_data)
        ));
    }
    Ok(())
}

pub async fn rename_onto_a_directory_link_replaces_the_link(
    fs: &dyn Filesystem,
) -> Result<(), String> {
    fs.write(Path::new("src"), b"NEW")
        .await
        .map_err(|e| format!("write src: {e}"))?;
    fs.mkdir(Path::new("dir"))
        .await
        .map_err(|e| format!("mkdir dir: {e}"))?;
    fs.symlink(Path::new("dir"), Path::new("link"))
        .await
        .map_err(|e| format!("symlink: {e}"))?;
    fs.rename(Path::new("src"), Path::new("link"))
        .await
        .map_err(|e| format!("rename(src, link): {e}"))?;

    let link_entry = fs
        .lstat(Path::new("link"))
        .await
        .map_err(|e| format!("lstat(link): {e}"))?;
    if !link_entry.is_file() {
        return Err(format!(
            "expected lstat(link) to be a regular file after rename onto it (POSIX rename does not follow the destination), got {:?}",
            link_entry.kind
        ));
    }

    let link_data = fs
        .read(Path::new("link"))
        .await
        .map_err(|e| format!("read(link): {e}"))?;
    if link_data != b"NEW" {
        return Err(format!(
            "expected read(link) == b\"NEW\", got {:?}",
            String::from_utf8_lossy(&link_data)
        ));
    }

    let dir_entry = fs
        .stat(Path::new("dir"))
        .await
        .map_err(|e| format!("stat(dir): {e}"))?;
    if !dir_entry.is_dir() {
        return Err(format!(
            "expected dir to still exist as a directory, got {:?}",
            dir_entry.kind
        ));
    }

    let listing = fs
        .list(Path::new("dir"))
        .await
        .map_err(|e| format!("list(dir): {e}"))?;
    if !listing.is_empty() {
        return Err(format!(
            "expected dir to remain empty (nothing moved into it), got {} entries",
            listing.len()
        ));
    }
    Ok(())
}

pub async fn write_through_a_file_link_updates_the_target(
    fs: &dyn Filesystem,
) -> Result<(), String> {
    fs.write(Path::new("target"), b"TARGET")
        .await
        .map_err(|e| format!("write target: {e}"))?;
    fs.symlink(Path::new("target"), Path::new("link"))
        .await
        .map_err(|e| format!("symlink: {e}"))?;
    fs.write(Path::new("link"), b"VIA LINK")
        .await
        .map_err(|e| format!("write(link): {e}"))?;

    let target_data = fs
        .read(Path::new("target"))
        .await
        .map_err(|e| format!("read(target): {e}"))?;
    if target_data != b"VIA LINK" {
        return Err(format!(
            "expected read(target) == b\"VIA LINK\" (write follows the link), got {:?}",
            String::from_utf8_lossy(&target_data)
        ));
    }

    let link_entry = fs
        .lstat(Path::new("link"))
        .await
        .map_err(|e| format!("lstat(link): {e}"))?;
    if !link_entry.is_symlink() {
        return Err(format!(
            "expected lstat(link) to still report symlink kind after write, got {:?}",
            link_entry.kind
        ));
    }
    Ok(())
}

pub async fn stat_on_a_link_loop_errors_instead_of_hanging(
    fs: &dyn Filesystem,
) -> Result<(), String> {
    fs.symlink(Path::new("b"), Path::new("a"))
        .await
        .map_err(|e| format!("symlink a -> b: {e}"))?;
    fs.symlink(Path::new("a"), Path::new("b"))
        .await
        .map_err(|e| format!("symlink b -> a: {e}"))?;

    match tokio::time::timeout(std::time::Duration::from_secs(2), fs.stat(Path::new("a"))).await {
        Ok(Ok(entry)) => Err(format!(
            "expected stat(a) to error on a symlink loop, got Ok({:?})",
            entry.kind
        )),
        Ok(Err(_)) => Ok(()),
        Err(_) => Err("stat(a) hung on a symlink loop instead of erroring".to_string()),
    }
}

pub async fn list_shows_a_link_as_a_link(fs: &dyn Filesystem) -> Result<(), String> {
    fs.write(Path::new("target"), b"TARGET")
        .await
        .map_err(|e| format!("write target: {e}"))?;
    fs.symlink(Path::new("target"), Path::new("link"))
        .await
        .map_err(|e| format!("symlink: {e}"))?;

    let entries = fs
        .list(Path::new(""))
        .await
        .map_err(|e| format!("list(\"\"): {e}"))?;
    let names: Vec<&str> = entries.iter().map(|e| e.name.as_str()).collect();

    let link_entry = entries
        .iter()
        .find(|e| e.name == "link")
        .ok_or_else(|| format!("expected an entry named \"link\" in list(\"\"), got {names:?}"))?;
    if !link_entry.is_symlink() {
        return Err(format!(
            "expected the \"link\" entry to be symlink kind, got {:?}",
            link_entry.kind
        ));
    }

    let target_entry = entries
        .iter()
        .find(|e| e.name == "target")
        .ok_or_else(|| {
            format!("expected an entry named \"target\" in list(\"\"), got {names:?}")
        })?;
    if !target_entry.is_file() {
        return Err(format!(
            "expected the \"target\" entry to be file kind, got {:?}",
            target_entry.kind
        ));
    }
    Ok(())
}

// Adapts an async case fn to the boxed-future `Case` fn-pointer shape. The
// local `adapt` fn is a fresh item per invocation, so names never collide.
macro_rules! case {
    ($name:ident) => {{
        fn adapt(
            fs: &dyn Filesystem,
        ) -> Pin<Box<dyn Future<Output = Result<(), String>> + Send + '_>> {
            Box::pin($name(fs))
        }
        (stringify!($name), adapt as Case)
    }};
}

pub const CASES: &[(&str, Case)] = &[
    case!(lstat_reports_the_link_itself),
    case!(read_link_returns_the_target_verbatim),
    case!(relative_target_resolves_from_the_link_directory),
    case!(dangling_link_is_visible_to_lstat_but_not_stat),
    case!(remove_unlinks_the_link_and_keeps_the_file),
    case!(remove_unlinks_a_link_to_a_directory),
    case!(rename_moves_the_link_not_the_target),
    case!(rename_onto_a_file_link_replaces_the_link),
    case!(rename_onto_a_dangling_link_replaces_the_link),
    case!(rename_onto_a_directory_link_replaces_the_link),
    case!(write_through_a_file_link_updates_the_target),
    case!(stat_on_a_link_loop_errors_instead_of_hanging),
    case!(list_shows_a_link_as_a_link),
];

/// Runs every case, each against its own fresh root from `make_root`.
pub async fn run_all<F, Fut>(make_root: F) -> Vec<(&'static str, Result<(), String>)>
where
    F: Fn() -> Fut,
    Fut: Future<Output = Box<dyn Filesystem>>,
{
    let mut results = Vec::with_capacity(CASES.len());
    for (name, case) in CASES {
        let root = make_root().await;
        results.push((*name, case(root.as_ref()).await));
    }
    results
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_no_failures(backend: &str, results: Vec<(&'static str, Result<(), String>)>) {
        let failures: Vec<String> = results
            .into_iter()
            .filter_map(|(name, outcome)| outcome.err().map(|msg| format!("{name}: {msg}")))
            .collect();
        if !failures.is_empty() {
            panic!(
                "{backend} conformance failures ({}):\n{}",
                failures.len(),
                failures.join("\n")
            );
        }
    }

    #[cfg(all(unix, feature = "localfs"))]
    #[tokio::test]
    async fn localfs_conformance() {
        use crate::local::LocalFs;

        // Hold every TempDir until the assertions run, so the roots are
        // removed on drop instead of leaking under /tmp.
        let kept = std::sync::Mutex::new(Vec::new());
        let results = run_all(|| async {
            let dir = tempfile::tempdir().expect("tempdir");
            let fs = LocalFs::new(dir.path());
            kept.lock().expect("tempdir list").push(dir);
            Box::new(fs) as Box<dyn Filesystem>
        })
        .await;
        assert_no_failures("LocalFs", results);
        drop(kept);
    }

    #[cfg(feature = "memory")]
    #[tokio::test]
    async fn memoryfs_conformance() {
        use crate::memory::MemoryFs;

        let results = run_all(|| async { Box::new(MemoryFs::new()) as Box<dyn Filesystem> }).await;
        assert_no_failures("MemoryFs", results);
    }

    #[cfg(all(feature = "overlay", feature = "memory"))]
    #[tokio::test]
    async fn overlayfs_conformance() {
        use crate::memory::MemoryFs;
        use crate::overlay::OverlayFs;
        use std::sync::Arc;

        let results = run_all(|| async {
            let lower: Arc<dyn Filesystem> = Arc::new(MemoryFs::new());
            Box::new(OverlayFs::over(lower)) as Box<dyn Filesystem>
        })
        .await;
        assert_no_failures("OverlayFs", results);
    }
}
