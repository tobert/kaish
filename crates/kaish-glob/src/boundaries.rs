//! Mount regions, for keeping a recursive walk inside the mount it starts in.

use std::path::{Path, PathBuf};

/// The mount points a recursive walk stays between.
///
/// Each boundary other than `/` starts a *region*: the boundary's subtree.
/// Regions do not nest. A boundary inside another boundary belongs to the
/// outer region, so with `/v` and `/v/jobs` both reported, `/v/jobs/1` is in
/// the `/v` region. Everything outside every region is in the root region.
///
/// A walk stays in the region of its start. A walk from `/` does not enter
/// `/v`, `/dev`, or a nested mount such as `/home/u/src`; a walk from `/v`
/// walks every mount under `/v`. `FileWalker` also enters the directories on
/// the path its pattern names, so a glob such as `/v/jobs/*` reaches
/// `/v/jobs` from a walk rooted at `/`.
///
/// An empty set has one region, so a walk over a filesystem that reports no
/// boundaries crosses everything.
#[derive(Debug, Clone, Default)]
pub struct WalkBoundaries {
    points: Vec<PathBuf>,
}

impl WalkBoundaries {
    /// Build the set from mount points. `/` and relative paths are ignored:
    /// `/` is the root region, and a relative path names no fixed place.
    pub fn new(points: impl IntoIterator<Item = PathBuf>) -> Self {
        let mut points: Vec<PathBuf> = points
            .into_iter()
            .filter(|p| p.is_absolute() && p.parent().is_some())
            .collect();
        // Shallowest first, so `region` finds the outermost boundary first.
        points.sort_by_key(|p| p.components().count());
        points.dedup();
        Self { points }
    }

    /// True when no boundary other than `/` was reported.
    pub fn is_empty(&self) -> bool {
        self.points.is_empty()
    }

    /// The region that holds `path`: the outermost boundary at or above it,
    /// or `None` for the root region.
    pub fn region(&self, path: &Path) -> Option<&Path> {
        self.points
            .iter()
            .find(|point| path.starts_with(point))
            .map(PathBuf::as_path)
    }

    /// Whether a walk that starts at `start` may descend into `dir`: true
    /// when both are in the same region. A walk that names its start through
    /// a pattern (`/v/jobs/*` walked from `/`) checks the directories on that
    /// path itself; see `FileWalker`.
    pub fn may_descend(&self, start: &Path, dir: &Path) -> bool {
        self.region(dir) == self.region(start)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn boundaries(points: &[&str]) -> WalkBoundaries {
        WalkBoundaries::new(points.iter().map(PathBuf::from))
    }

    #[test]
    fn root_and_relative_points_are_not_regions() {
        let set = boundaries(&["/", "relative"]);
        assert!(set.is_empty());
        assert_eq!(set.region(Path::new("/v")), None);
    }

    #[test]
    fn nested_boundary_belongs_to_the_outer_region() {
        let set = boundaries(&["/v/jobs", "/", "/v", "/dev"]);
        assert_eq!(set.region(Path::new("/v/jobs/1")), Some(Path::new("/v")));
        assert_eq!(set.region(Path::new("/v")), Some(Path::new("/v")));
        assert_eq!(set.region(Path::new("/dev/null")), Some(Path::new("/dev")));
        assert_eq!(set.region(Path::new("/home")), None);
    }

    #[test]
    fn region_matches_whole_components() {
        let set = boundaries(&["/v"]);
        assert_eq!(set.region(Path::new("/var/log")), None);
    }

    #[test]
    fn walk_from_root_does_not_enter_a_region() {
        let set = boundaries(&["/", "/v", "/dev"]);
        let root = Path::new("/");
        assert!(set.may_descend(root, Path::new("/home")));
        assert!(!set.may_descend(root, Path::new("/v")));
        assert!(!set.may_descend(root, Path::new("/dev")));
    }

    #[test]
    fn walk_inside_a_region_enters_its_nested_mounts() {
        let set = boundaries(&["/", "/v", "/v/jobs"]);
        assert!(set.may_descend(Path::new("/v"), Path::new("/v/jobs")));
    }

    #[test]
    fn walk_inside_a_region_does_not_leave_it() {
        let set = boundaries(&["/v", "/r"]);
        // A followed symlink can point anywhere; its real path decides.
        assert!(!set.may_descend(Path::new("/v"), Path::new("/r/share")));
        assert!(!set.may_descend(Path::new("/v"), Path::new("/home")));
    }

    #[test]
    fn empty_set_crosses_everything() {
        let set = WalkBoundaries::default();
        assert!(set.may_descend(Path::new("/"), Path::new("/v")));
    }
}
