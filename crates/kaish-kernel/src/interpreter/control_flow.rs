//! Control flow signals for loops and functions.
//!
//! These types allow break, continue, return, and exit to propagate
//! through the statement execution stack.

use super::result::ExecResult;

/// Control flow signal from statement execution.
///
/// Normal execution returns `Normal(result)`. Loop control uses `Break` and `Continue`.
/// Function returns use `Return`, and script exits use `Exit`.
#[derive(Debug, Clone)]
pub enum ControlFlow {
    /// Normal completion with a result.
    Normal(ExecResult),
    /// Break out of loop(s). `levels` indicates how many loops to break out of.
    Break { levels: usize, result: ExecResult },
    /// Continue to next iteration of loop(s). `levels` indicates how many loops to skip.
    Continue { levels: usize, result: ExecResult },
    /// Return from a function with a result.
    Return { value: ExecResult },
    /// Exit the entire script with an exit code.
    ///
    /// `result` carries the output produced before the exit. An immediate exit
    /// stops the script; it does not discard the iterations that already ran.
    /// `code` stays authoritative for the script's status — `result.code` is
    /// never read.
    Exit { code: i64, result: ExecResult },
}

impl ControlFlow {
    /// Create a normal control flow with a successful result.
    pub fn ok(result: ExecResult) -> Self {
        ControlFlow::Normal(result)
    }

    /// Create a break with 1 level.
    pub fn break_one() -> Self {
        ControlFlow::Break {
            levels: 1,
            result: ExecResult::success(""),
        }
    }

    /// Create a break with n levels.
    pub fn break_n(n: usize) -> Self {
        ControlFlow::Break {
            levels: n,
            result: ExecResult::success(""),
        }
    }

    /// Create a continue with 1 level.
    pub fn continue_one() -> Self {
        ControlFlow::Continue {
            levels: 1,
            result: ExecResult::success(""),
        }
    }

    /// Create a continue with n levels.
    pub fn continue_n(n: usize) -> Self {
        ControlFlow::Continue {
            levels: n,
            result: ExecResult::success(""),
        }
    }

    /// Create a return with a value.
    pub fn return_value(value: ExecResult) -> Self {
        ControlFlow::Return { value }
    }

    /// Create an exit with a code, carrying no output yet.
    ///
    /// Each loop the exit passes through folds its own accumulated output in,
    /// so the output arrives with the signal rather than being left behind.
    pub fn exit_code(code: i64) -> Self {
        ControlFlow::Exit {
            code,
            result: ExecResult::success(""),
        }
    }

    /// Check if this is normal flow.
    pub fn is_normal(&self) -> bool {
        matches!(self, ControlFlow::Normal(_))
    }

    /// Get the result if this is normal flow.
    pub fn into_result(self) -> Option<ExecResult> {
        match self {
            ControlFlow::Normal(r) => Some(r),
            _ => None,
        }
    }

    /// Decrement break/continue levels by 1 and return whether we should stop here.
    ///
    /// Returns `true` if the break/continue should be handled at this level,
    /// `false` if it should propagate further.
    pub fn decrement_level(&mut self) -> bool {
        match self {
            ControlFlow::Break { levels, .. } | ControlFlow::Continue { levels, .. } => {
                if *levels <= 1 {
                    true
                } else {
                    *levels -= 1;
                    false
                }
            }
            _ => false,
        }
    }
}

impl Default for ControlFlow {
    fn default() -> Self {
        ControlFlow::Normal(ExecResult::success(""))
    }
}

impl From<ExecResult> for ControlFlow {
    fn from(result: ExecResult) -> Self {
        ControlFlow::Normal(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normal_flow() {
        let flow = ControlFlow::ok(ExecResult::success("test"));
        assert!(flow.is_normal());
    }

    #[test]
    fn test_break_decrement() {
        let mut flow = ControlFlow::break_n(3);
        assert!(!flow.decrement_level()); // 3 -> 2
        assert!(!flow.decrement_level()); // 2 -> 1
        assert!(flow.decrement_level()); // 1 -> should stop
    }

    #[test]
    fn test_continue_decrement() {
        let mut flow = ControlFlow::continue_n(2);
        assert!(!flow.decrement_level()); // 2 -> 1
        assert!(flow.decrement_level()); // 1 -> should stop
    }

    #[test]
    fn test_from_exec_result() {
        let result = ExecResult::success("hello");
        let flow: ControlFlow = result.into();
        assert!(flow.is_normal());
    }
}
