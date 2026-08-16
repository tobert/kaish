//! Walk a script the way an embedder would: find which interpreter reads
//! each heredoc, take the program with the shell framing off, and close the
//! gap on the bodies that expand.
//!
//! Run with `cargo run -p kaish-kernel --example heredoc_demo`.

use std::process::ExitCode;

use kaish_kernel::{expand_fragment, plan_program, Expansion, FragmentAddr};
use kaish_types::Value;

const SCRIPT: &str = r#"python3 <<'PY'
import os
print(os.getcwd())
PY
sqlite3 db <<SQL
select * from t where user = '${USER}';
SQL
python3 <<PY
stamp = "$(date +%s)"
PY"#;

fn main() -> ExitCode {
    // The values this caller holds and is willing to judge against. Nothing
    // is read from session state, so these are the only ones in play.
    let scope = vec![("USER".to_string(), Value::String("amy".to_string()))];

    let statements = match plan_program(SCRIPT) {
        Ok(statements) => statements,
        Err(errors) => {
            eprintln!("does not parse: {} error(s)", errors.len());
            return ExitCode::FAILURE;
        }
    };

    for planned in statements {
        for command in &planned.plan.commands {
            for heredoc in &command.heredocs {
                println!(
                    "── {} <<{}  (literal={}, reads {:?})",
                    command.name, heredoc.delimiter, heredoc.literal, heredoc.free_variables,
                );
                let addr = FragmentAddr::new(planned.index, heredoc.index);
                match expand_fragment(SCRIPT, addr, &scope) {
                    Ok(Expansion::Complete(text)) => println!("reads on stdin:\n{text}"),
                    // The substitution is not run here. A caller that judges
                    // it safe runs it in a kernel of its own construction and
                    // expands again with the answer in scope.
                    Ok(Expansion::Blocked { holes }) => {
                        for hole in holes {
                            println!(
                                "blocked on {} — it would run: {}",
                                hole.source, hole.plans[0].rendered,
                            );
                        }
                    }
                    Err(error) => println!("cannot expand: {error}"),
                }
            }
        }
    }
    ExitCode::SUCCESS
}
