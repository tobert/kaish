//! `ls -l`, `ls -a`, and `stat` print GNU's text formats. On Linux the
//! listing is compared byte for byte with GNU `ls` in the C locale and UTC.

#![cfg(feature = "localfs")]
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use common::kernel_at;
use std::fs;
use std::time::{Duration, SystemTime};

/// The tree lives one level down, so `ls -a`'s `..` is private to the test
/// rather than a shared temp directory that changes under parallel tests.
struct Fixture {
    _tmp: tempfile::TempDir,
    root: std::path::PathBuf,
}

impl Fixture {
    fn path(&self) -> &std::path::Path {
        &self.root
    }
}

fn fixture() -> Fixture {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("root");
    fs::create_dir(&root).unwrap();
    let root_path = root.clone();
    let root = root_path.as_path();
    fs::create_dir(root.join("sub")).unwrap();
    fs::write(root.join("sub").join("inner"), b"in\n").unwrap();
    fs::write(root.join("a.txt"), b"hello\n").unwrap();
    fs::write(root.join(".hidden"), b"").unwrap();
    fs::write(root.join("big"), vec![b'x'; 5000]).unwrap();
    // An mtime older than six months prints the year instead of the time.
    let old = SystemTime::UNIX_EPOCH + Duration::from_secs(1_577_934_245); // 2020-01-02 03:04:05 UTC
    fs::File::options().write(true).open(root.join("big")).unwrap().set_modified(old).unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::{symlink, PermissionsExt};
        symlink("a.txt", root.join("link")).unwrap();
        symlink("nowhere", root.join("dangling")).unwrap();
        fs::set_permissions(root.join("a.txt"), fs::Permissions::from_mode(0o4755)).unwrap();
    }
    Fixture { _tmp: tmp, root: root_path }
}

async fn kaish(root: &std::path::Path, script: &str) -> (String, String, i64) {
    let kernel = kernel_at(root);
    let r = kernel.execute(&format!("export TZ=UTC; {script}")).await.expect("execute");
    (r.text_out().into_owned(), r.err.clone(), r.code)
}

#[cfg(target_os = "linux")]
fn gnu(root: &std::path::Path, script: &str) -> String {
    gnu_in(root, "UTC", script)
}

#[cfg(target_os = "linux")]
fn gnu_in(root: &std::path::Path, tz: &str, script: &str) -> String {
    let out = std::process::Command::new("sh")
        .arg("-c")
        .arg(script)
        .env("LC_ALL", "C")
        .env("TZ", tz)
        .current_dir(root)
        .output()
        .unwrap();
    String::from_utf8(out.stdout).unwrap()
}

#[cfg(target_os = "linux")]
#[rstest::rstest]
#[case("ls -l")]
#[case("ls -la")]
#[case("ls -lA")]
#[case("ls -lh")]
#[case("ls -lS")]
#[case("ls -lR")]
#[case("ls -lRa")]
#[case("ls -l a.txt link dangling")]
#[case("ls -ld sub")]
#[case("ls")]
#[case("ls -a")]
#[case("ls -A")]
#[case("ls -R")]
#[tokio::test]
async fn ls_matches_gnu(#[case] script: &str) {
    let tmp = fixture();
    let (out, err, code) = kaish(tmp.path(), script).await;
    assert_eq!(code, 0, "{script}: {err}");
    assert_eq!(out, gnu(tmp.path(), script), "{script}");
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn stat_lines_match_gnu() {
    let tmp = fixture();
    let (out, err, code) = kaish(tmp.path(), "stat a.txt link").await;
    assert_eq!(code, 0, "{err}");
    // Some coreutils releases quote a link's names (`'link' -> 'a.txt'`,
    // Ubuntu 24.04); kaish prints them bare, as coreutils on Arch does.
    let expected = gnu(tmp.path(), "stat a.txt link").replace('\'', "");
    // kaish prints GNU's lines for the fields it has, in GNU's order. GNU's
    // `IO Block:` field and its device, inode, and other time lines are absent.
    for line in out.lines() {
        let line = line.replace(" Links:", "Links:");
        if line.starts_with("  Size:") {
            // `  Size: 6         \tBlocks: 8          regular file`
            let after_blocks = line.find("Blocks: ").unwrap() + "Blocks: ".len();
            let number_end = after_blocks + line[after_blocks..].find(' ').unwrap();
            let (head, kind) = (&line[..number_end], line[number_end..].trim());
            assert!(
                expected.lines().any(|l| l.starts_with(head) && l.ends_with(kind)),
                "{line:?} in {expected}"
            );
        } else if line.starts_with("Links:") {
            assert!(expected.contains(&format!("  {line}\n")), "{line:?} in {expected}");
        } else {
            assert!(expected.lines().any(|l| l == line), "{line:?} in {expected}");
        }
    }
    assert_eq!(out.lines().filter(|l| l.starts_with("  File:")).count(), 2, "{out}");
    // Five lines per file, in GNU's order, with no blank line between files.
    let starts: Vec<&str> = out.lines().map(|l| &l[..6]).collect();
    assert_eq!(starts, ["  File", "  Size", " Links", "Access", "Modify"].repeat(2), "{out}");
}

#[tokio::test]
async fn ls_a_never_descends_into_dot_entries() {
    let tmp = fixture();
    let (out, err, code) = kaish(tmp.path(), "ls -Ra").await;
    assert_eq!(code, 0, "{err}");
    let headers: Vec<&str> = out.lines().filter(|l| l.ends_with(':')).collect();
    assert_eq!(headers, [".:", "./sub:"], "{out}");
}

#[tokio::test]
async fn ls_l_prints_question_marks_for_fields_memoryfs_does_not_track() {
    let tmp = fixture();
    let (out, err, code) = kaish(tmp.path(), "echo hi > /v/x.txt; ls -l /v/x.txt").await;
    assert_eq!(code, 0, "{err}");
    let fields: Vec<&str> = out.split_whitespace().collect();
    assert_eq!(fields[..5], ["-rw-rw-rw-", "?", "?", "?", "3"], "{out}");
    assert_eq!(fields.last(), Some(&"/v/x.txt"), "{out}");
}

#[tokio::test]
async fn ls_l_with_an_unknown_tz_names_the_fix() {
    let tmp = fixture();
    let kernel = kernel_at(tmp.path());
    let r = kernel.execute("export TZ=Mars/Olympus; ls -l").await.expect("execute");
    assert_eq!(r.code, 1);
    assert!(r.err.contains("unknown timezone 'Mars/Olympus'") && r.err.contains("Asia/Tokyo"), "{}", r.err);
}

#[tokio::test]
async fn date_prints_the_gnu_default_with_the_zone_abbreviation() {
    let tmp = fixture();
    let script = "date -d @1700000000; TZ=Asia/Tokyo date -d @1700000000; TZ=America/Sao_Paulo date -d @1700000000";
    let (out, err, code) = kaish(tmp.path(), script).await;
    assert_eq!(code, 0, "{err}");
    // tzdata names Sao Paulo's zone `-03`; GNU prints it as is.
    assert_eq!(
        out,
        "Tue Nov 14 22:13:20 UTC 2023\nWed Nov 15 07:13:20 JST 2023\nTue Nov 14 19:13:20 -03 2023\n"
    );
}

#[tokio::test]
async fn date_pads_a_zone_abbreviation_to_a_width() {
    let tmp = fixture();
    let (out, err, code) = kaish(tmp.path(), "TZ=Asia/Tokyo date -d @1700000000 '+[%10Z] [%Z] [%%Z]'").await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(out, "[       JST] [JST] [%Z]\n");
    let (_, err, code) = kaish(tmp.path(), "date '+%99999Z'").await;
    assert_eq!(code, 2);
    assert!(err.contains("width 99999 in %99999Z is too large (at most 4096)"), "{err}");
}

#[cfg(target_os = "linux")]
#[rstest::rstest]
#[case(":/etc/localtime")]
#[case(":UTC")]
#[case("UTC0")]
#[case("JST-9")]
#[case("<+0530>-5:30")]
#[case("/usr/share/zoneinfo/Asia/Tokyo")]
#[tokio::test]
async fn ls_l_reads_tz_as_the_c_library_does(#[case] tz: &str) {
    let tmp = fixture();
    let kernel = kernel_at(tmp.path());
    let r = kernel.execute(&format!("export TZ='{tz}'; ls -l big a.txt")).await.expect("execute");
    assert_eq!(r.code, 0, "{tz}: {}", r.err);
    assert_eq!(r.text_out(), gnu_in(tmp.path(), tz, "ls -l big a.txt"), "{tz}");
}

#[tokio::test]
async fn a_tz_with_daylight_saving_rules_names_the_iana_fix() {
    let tmp = fixture();
    let kernel = kernel_at(tmp.path());
    let r = kernel.execute("export TZ='EST5EDT,M3.2.0,M11.1.0'; stat a.txt").await.expect("execute");
    assert_eq!(r.code, 1);
    assert!(r.err.contains("daylight-saving rules") && r.err.contains("America/New_York"), "{}", r.err);
}

#[tokio::test]
async fn ls_a_json_includes_dot_entries() {
    let tmp = fixture();
    let (out, err, code) = kaish(tmp.path(), "ls -a sub --json").await;
    assert_eq!(code, 0, "{err}");
    let json: serde_json::Value = serde_json::from_str(&out).expect("valid json");
    assert_eq!(json, serde_json::json!([".", "..", "inner"]), "{json}");
}

#[tokio::test]
async fn stat_names_an_empty_file_in_text_json_and_format() {
    let tmp = fixture();
    let (out, err, code) = kaish(tmp.path(), "stat .hidden --json").await;
    assert_eq!(code, 0, "{err}");
    let json: serde_json::Value = serde_json::from_str(&out).expect("valid json");
    assert_eq!(json[0]["TYPE"], "regular empty file", "{json}");
    let (out, _, _) = kaish(tmp.path(), "stat -c %F .hidden; stat -c %F a.txt").await;
    assert_eq!(out, "regular empty file\nregular file\n");
}

#[tokio::test]
async fn stat_prints_question_marks_for_fields_memoryfs_does_not_track() {
    let tmp = fixture();
    let (out, err, code) = kaish(tmp.path(), "echo hi > /v/x.txt; stat /v/x.txt").await;
    assert_eq!(code, 0, "{err}");
    assert!(out.contains("Blocks: ?"), "{out}");
    assert!(out.contains(" Links: ?\n"), "{out}");
    assert!(out.contains("Uid: (    ?/       ?)   Gid: (    ?/       ?)"), "{out}");
}
