//! System trash backend wrapping the `trash` crate.
//!
//! This is the default implementation — identical to the previous inline behavior,
//! just behind the `TrashBackend` trait.

use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};

use async_trait::async_trait;

use crate::trash::{TrashBackend, TrashEntry, TrashError, TrashId, find_restore_match};

/// System trash backend using the freedesktop.org / platform trash via the `trash` crate.
pub struct SystemTrash;

impl SystemTrash {
    /// Convert a `trash::TrashItem` to our `TrashEntry`.
    fn to_entry(item: &trash::TrashItem) -> TrashEntry {
        TrashEntry {
            id: TrashId::system(item.id.clone()),
            name: item.name.to_string_lossy().to_string(),
            original_path: item.original_parent.join(&item.name),
            deleted_at: item.time_deleted,
        }
    }
}

/// Run a blocking trash operation, flattening JoinError/trash::Error.
async fn spawn_trash<F, T>(op: F) -> Result<T, TrashError>
where
    F: FnOnce() -> Result<T, trash::Error> + Send + 'static,
    T: Send + 'static,
{
    match tokio::task::spawn_blocking(op).await {
        Ok(Ok(v)) => Ok(v),
        Ok(Err(e)) => Err(TrashError::Backend(e.to_string())),
        Err(e) => Err(TrashError::Join(e.to_string())),
    }
}

#[async_trait]
impl TrashBackend for SystemTrash {
    async fn trash(&self, path: &Path) -> Result<(), TrashError> {
        let p = path.to_owned();
        spawn_trash(move || trash::delete(&p)).await
    }

    async fn trash_bytes(&self, original_path: &Path, bytes: &[u8]) -> Result<(), TrashError> {
        // The platform trash moves real files, so materialize the snapshot to a
        // staging file (basename preserved for a recognizable entry) under a
        // unique dir, then trash it. Restore lands at the staging path — the
        // bytes are what's recoverable, not the location.
        //
        // Stage under the persistent data dir, NOT /tmp: a tmpfs /tmp would put
        // the trash at /tmp/.Trash-$UID and lose the snapshot on reboot — the
        // opposite of a safety net. Staging on the home partition lands it in
        // the durable ~/.local/share/Trash and keeps the move on one filesystem.
        let basename = original_path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "snapshot".to_string());
        // nanos alone collides under parallel agents / tight loops; mix in a
        // process-global counter so two snapshots in the same nanosecond can't
        // race on the same staging path.
        static SEQ: AtomicU64 = AtomicU64::new(0);
        let nanos = kaish_types::clock::system_now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let seq = SEQ.fetch_add(1, Ordering::Relaxed);
        let dir = crate::paths::data_dir()
            .join("trash-staging")
            .join(format!("{nanos:x}-{seq:x}"));
        let staged = dir.join(&basename);
        tokio::fs::create_dir_all(&dir)
            .await
            .map_err(|e| TrashError::Backend(e.to_string()))?;
        tokio::fs::write(&staged, bytes)
            .await
            .map_err(|e| TrashError::Backend(e.to_string()))?;
        let result = self.trash(&staged).await;
        // The staged file is now in the trash; the staging dir is empty. Drop it
        // so repeated overwrites don't leak empty dirs under the data dir.
        tokio::fs::remove_dir(&dir).await.ok();
        result
    }

    async fn list(&self, filter: Option<&str>) -> Result<Vec<TrashEntry>, TrashError> {
        let items = spawn_trash(trash::os_limited::list).await?;
        let filter_owned = filter.map(|s| s.to_owned());

        let entries: Vec<TrashEntry> = items
            .iter()
            .filter(|item| {
                if let Some(ref f) = filter_owned {
                    item.name.to_string_lossy().contains(f.as_str())
                } else {
                    true
                }
            })
            .map(Self::to_entry)
            .collect();

        Ok(entries)
    }

    async fn find_by_name(&self, name: &str) -> Result<Vec<TrashEntry>, TrashError> {
        let items = spawn_trash(trash::os_limited::list).await?;

        let named_items: Vec<(String, &trash::TrashItem)> = items
            .iter()
            .map(|item| (item.name.to_string_lossy().to_string(), item))
            .collect();

        find_restore_match(named_items, name)
            .map(|matched| matched.into_iter().map(Self::to_entry).collect())
            .map_err(TrashError::Backend)
    }

    async fn restore(&self, entries: Vec<TrashEntry>) -> Result<(), TrashError> {
        // Reconstruct trash::TrashItem from our TrashEntry
        let items: Vec<trash::TrashItem> = entries
            .into_iter()
            .map(|entry| {
                let crate::trash::TrashIdInner::System(id) = entry.id.0;
                trash::TrashItem {
                    id,
                    name: entry.name.into(),
                    original_parent: entry
                        .original_path
                        .parent()
                        .unwrap_or(Path::new("/"))
                        .to_path_buf(),
                    time_deleted: entry.deleted_at,
                }
            })
            .collect();

        spawn_trash(move || trash::os_limited::restore_all(items)).await
    }

    async fn purge_all(&self) -> Result<usize, TrashError> {
        let items = spawn_trash(trash::os_limited::list).await?;
        if items.is_empty() {
            return Ok(0);
        }
        let count = items.len();
        spawn_trash(move || trash::os_limited::purge_all(items)).await?;
        Ok(count)
    }
}
