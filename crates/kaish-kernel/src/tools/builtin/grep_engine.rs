//! Search engine glue for the `grep` builtin.
//!
//! Wraps `grep_searcher::Searcher` + `grep_matcher::Matcher` with an
//! accumulator [`Sink`] that emits a linear stream of [`SearchEvent`]s,
//! which `grep` renders into kaish's `OutputData` shapes.

use std::path::PathBuf;

use bstr::ByteSlice;
use grep_matcher::Matcher;
use grep_searcher::{Searcher, Sink, SinkContext, SinkContextKind, SinkMatch};

/// One line of structured match data. Multi-line matches collapse into a
/// single record whose `line_text` may span newlines (caller decides how
/// to render).
#[derive(Debug, Clone)]
pub struct MatchRecord {
    /// Source path. `None` for stdin / in-memory input.
    pub path: Option<PathBuf>,
    /// Line number of the first matched line, when line numbering is on.
    pub line_number: Option<u64>,
    /// Absolute byte offset of the start of the matched region in the
    /// input. Useful for `--json` consumers; not always meaningful as a
    /// slice index when mmap or streaming is in play.
    pub absolute_byte_offset: u64,
    /// UTF-8 text of the matched line(s). Lossy-decoded from raw bytes.
    pub line_text: String,
    /// Byte ranges within `line_text` for each submatch the matcher
    /// reported on this line.
    pub submatches: Vec<Submatch>,
}

/// One submatch within a matched line.
#[derive(Debug, Clone)]
pub struct Submatch {
    pub text: String,
    pub start: usize,
    pub end: usize,
}

/// Reason a context line was emitted.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContextKind {
    Before,
    After,
    Other,
}

impl From<&SinkContextKind> for ContextKind {
    fn from(k: &SinkContextKind) -> Self {
        match k {
            SinkContextKind::Before => ContextKind::Before,
            SinkContextKind::After => ContextKind::After,
            SinkContextKind::Other => ContextKind::Other,
        }
    }
}

/// One non-matching line that was emitted as before/after context.
#[derive(Debug, Clone)]
pub struct ContextRecord {
    pub line_number: Option<u64>,
    pub line_text: String,
    pub kind: ContextKind,
}

/// Linear stream of events produced by a single `Searcher` run.
#[derive(Debug, Clone)]
pub enum SearchEvent {
    Match(MatchRecord),
    Context(ContextRecord),
    /// A break between non-contiguous context groups. Mirrors grep's `--`
    /// separator.
    ContextBreak,
}

/// Sink that accumulates matches/context into a `Vec<SearchEvent>`.
/// `path` is captured up-front so multi-file callers can tag records.
pub struct AccumulatorSink<'m, M: Matcher> {
    path: Option<PathBuf>,
    matcher: &'m M,
    events: Vec<SearchEvent>,
}

impl<'m, M: Matcher> AccumulatorSink<'m, M> {
    pub fn new(matcher: &'m M, path: Option<PathBuf>) -> Self {
        Self {
            path,
            matcher,
            events: Vec::new(),
        }
    }

    pub fn into_events(self) -> Vec<SearchEvent> {
        self.events
    }

    fn submatches_for(&self, line: &[u8]) -> Vec<Submatch> {
        let mut out = Vec::new();
        // `find_iter` walks every match in this line. We only swallow the
        // matcher's own internal errors silently — they shouldn't happen
        // on a slice that already matched, and a panic mid-Sink would tear
        // down the whole search.
        let _ = self.matcher.find_iter(line, |m| {
            let start = m.start();
            let end = m.end();
            let bytes = &line[start..end];
            out.push(Submatch {
                text: String::from_utf8_lossy(bytes).into_owned(),
                start,
                end,
            });
            true
        });
        out
    }
}

impl<M: Matcher> Sink for AccumulatorSink<'_, M> {
    type Error = std::io::Error;

    fn matched(
        &mut self,
        _searcher: &Searcher,
        mat: &SinkMatch<'_>,
    ) -> Result<bool, Self::Error> {
        let bytes = mat.bytes();
        let trimmed = trim_line_terminator(bytes);
        let line_text = trimmed.to_str_lossy().into_owned();
        let submatches = self.submatches_for(trimmed);
        self.events.push(SearchEvent::Match(MatchRecord {
            path: self.path.clone(),
            line_number: mat.line_number(),
            absolute_byte_offset: mat.absolute_byte_offset(),
            line_text,
            submatches,
        }));
        Ok(true)
    }

    fn context(
        &mut self,
        _searcher: &Searcher,
        ctx: &SinkContext<'_>,
    ) -> Result<bool, Self::Error> {
        let bytes = ctx.bytes();
        let line_text = trim_line_terminator(bytes).to_str_lossy().into_owned();
        self.events.push(SearchEvent::Context(ContextRecord {
            line_number: ctx.line_number(),
            line_text,
            kind: ctx.kind().into(),
        }));
        Ok(true)
    }

    fn context_break(&mut self, _searcher: &Searcher) -> Result<bool, Self::Error> {
        self.events.push(SearchEvent::ContextBreak);
        Ok(true)
    }
}

/// Stripped of trailing `\n` (and one `\r` if it precedes that `\n`).
fn trim_line_terminator(bytes: &[u8]) -> &[u8] {
    let bytes = bytes.strip_suffix(b"\n").unwrap_or(bytes);
    bytes.strip_suffix(b"\r").unwrap_or(bytes)
}


#[cfg(test)]
mod tests {
    use super::*;
    use grep_regex::RegexMatcher;
    use grep_searcher::SearcherBuilder;

    #[test]
    fn accumulator_collects_matches_with_submatches_and_line_numbers() {
        let input = b"alpha\nbeta foo bar\ngamma foo\nfoo foo\n";
        let matcher = RegexMatcher::new("foo").unwrap();
        let mut sink = AccumulatorSink::new(&matcher, Some(PathBuf::from("/v/test")));
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_slice(&matcher, input, &mut sink)
            .expect("search");

        let events = sink.into_events();
        // Expect 3 match events (line 2, line 3, line 4).
        let matches: Vec<&MatchRecord> = events
            .iter()
            .filter_map(|e| match e {
                SearchEvent::Match(m) => Some(m),
                _ => None,
            })
            .collect();

        assert_eq!(matches.len(), 3, "got events: {events:#?}");

        assert_eq!(matches[0].line_number, Some(2));
        assert_eq!(matches[0].line_text, "beta foo bar");
        assert_eq!(matches[0].submatches.len(), 1);
        assert_eq!(matches[0].submatches[0].text, "foo");
        assert_eq!(matches[0].submatches[0].start, 5);
        assert_eq!(matches[0].submatches[0].end, 8);

        assert_eq!(matches[1].line_number, Some(3));
        assert_eq!(matches[1].line_text, "gamma foo");

        // Line 4 has two "foo" submatches.
        assert_eq!(matches[2].line_number, Some(4));
        assert_eq!(matches[2].line_text, "foo foo");
        assert_eq!(matches[2].submatches.len(), 2);
        assert_eq!(matches[2].submatches[0].start, 0);
        assert_eq!(matches[2].submatches[1].start, 4);
    }

    #[test]
    fn accumulator_emits_context_lines() {
        let input = b"a\nb\nfoo\nc\nd\n";
        let matcher = RegexMatcher::new("foo").unwrap();
        let mut sink = AccumulatorSink::new(&matcher, None);
        SearcherBuilder::new()
            .line_number(true)
            .before_context(1)
            .after_context(1)
            .build()
            .search_slice(&matcher, input, &mut sink)
            .expect("search");

        let events = sink.into_events();
        // Expect: Context(b, before), Match(foo), Context(c, after).
        let summary: Vec<String> = events
            .iter()
            .map(|e| match e {
                SearchEvent::Match(m) => format!("M:{}", m.line_text),
                SearchEvent::Context(c) => format!("C:{:?}:{}", c.kind, c.line_text),
                SearchEvent::ContextBreak => "BREAK".to_string(),
            })
            .collect();
        assert_eq!(
            summary,
            vec![
                "C:Before:b".to_string(),
                "M:foo".to_string(),
                "C:After:c".to_string(),
            ]
        );
    }

    #[test]
    fn accumulator_strips_crlf_terminator() {
        let input = b"hello foo\r\nbye\r\n";
        let matcher = RegexMatcher::new("foo").unwrap();
        let mut sink = AccumulatorSink::new(&matcher, None);
        SearcherBuilder::new()
            .line_number(true)
            .build()
            .search_slice(&matcher, input, &mut sink)
            .expect("search");
        let events = sink.into_events();
        let SearchEvent::Match(m) = &events[0] else {
            panic!("expected match");
        };
        assert_eq!(m.line_text, "hello foo");
    }
}
