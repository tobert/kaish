# kaish Formal Grammar

This document defines kaish syntax in a way that's:
1. Unambiguous (every input has exactly one parse, or is an error)
2. LL(k) friendly (predictable with limited lookahead)
3. Testable (every production rule can be exercised in isolation)

## Design Principles

### 1. Bourne-Compatible Where Possible

```bash
NAME="value"           # assignment (bash-style)
local NAME="value"     # scoped assignment
tool NAME ...          # tool def starts with 'tool' or 'function'
if COND ...            # conditional starts with 'if'
while COND ...         # while loop starts with 'while'
for VAR ...            # for loop starts with 'for'
IDENT ...              # command starts with identifier
```

**ShellCheck Validation:** The grammar is designed so that valid kaish code
(excluding extensions) would pass `shellcheck --enable=all`. Constructs that
trigger ShellCheck warnings are not part of the grammar:
- No unquoted variable expansion contexts that could word-split
- No glob-sensitive contexts
- No deprecated syntax (backticks, single-bracket tests)

### 2. Explicit Delimiters

```bash
${VAR}                    # braced variables for paths/indices
$VAR                      # simple variables for identifiers
{body}                    # tool bodies in braces
"string"                  # double quotes - interpolation
'string'                  # single quotes - literal
```

### 3. No Implicit Operations

```bash
# Bash: implicit word splitting, glob expansion
echo $FILES        # might expand to multiple args!

# kaish: explicit everything
echo $FILES        # interpolates to single value
glob pattern="*.rs" | scatter | process $ITEM  # explicit glob tool
```

### 4. Simple Whitespace Rules

- Whitespace separates tokens
- Newlines end statements (except after `\` or inside `{}[]`)
- Inside quotes, whitespace is literal

---

## EBNF Grammar

```ebnf
(* === Top Level === *)

program     = { statement } ;
statement   = statement_chain , [ ";" | NEWLINE ] ;

(* Statement chaining - && and || at statement level *)
statement_chain = base_statement , { ( "&&" | "||" ) , base_statement } ;

base_statement = set_command                          (* set -e, set +e *)
               | assignment                           (* NAME=value, local NAME=value *)
               | tool_def
               | if_stmt
               | for_stmt
               | while_stmt
               | case_stmt
               | control_stmt
               | test_expr_stmt                       (* [[ ... ]] as statement *)
               | pipeline
               | NEWLINE
               ;

(* === set command vs assignment disambiguation === *)
(*
 * The parser uses lookahead to distinguish:
 *   set -e           → command (flag follows 'set')
 *   set +e           → command (plus-flag follows 'set')
 *   set              → command (nothing follows, or && || ; newline)
 *   set X = value    → assignment (IDENT follows 'set')
 *)
set_command = "set" , { flag_arg | IDENT } ;         (* set -e -u, set +e *)

(* === Statements === *)

(* Assignment - bash-style *)
assignment  = IDENT , "=" , value                    (* NAME="value" - no spaces *)
            | "local" , IDENT , "=" , value          (* local NAME = value - spaces OK *)
            | "set" , IDENT , "=" , value            (* set NAME = value - legacy form *)
            ;

tool_def    = ( "tool" | "function" ) , IDENT , { param_def } , "{" , { statement } , "}" ;
param_def   = IDENT , ":" , TYPE , [ "=" , literal ] ;

if_stmt     = "if" , condition , ";" , "then" , { statement } ,
              { "elif" , condition , ";" , "then" , { statement } } ,
              [ "else" , { statement } ] , "fi" ;

for_stmt    = "for" , IDENT , "in" , value , [ ";" ] , "do" , { statement } , "done" ;

while_stmt  = "while" , condition , [ ";" ] , "do" , { statement } , "done" ;

case_stmt   = "case" , value , "in" , { case_branch } , "esac" ;
case_branch = [ "(" ] , case_patterns , ")" , { statement } , ";;" ;
case_patterns = STRING , { "|" , STRING } ;          (* glob patterns supported: *, ?, [...] *)

control_stmt = "break" , [ INT ]                     (* break [n] - exit n loop levels *)
             | "continue" , [ INT ]                  (* continue [n] - skip n levels *)
             | "return" , [ value ]                  (* return [val] - from tool *)
             | "exit" , [ value ]                    (* exit [code] - from script *)
             ;

pipeline    = command , { "|" , command } , [ "&" ] , [ redirect ] ;

(* Command names: identifiers, 'true', 'false', '.' (source alias), 'source' *)
command     = command_name , { argument } ;
command_name = IDENT | "true" | "false" | "." | "source" ;

(* === Arguments === *)

argument    = long_flag_with_value                   (* --name=value *)
            | long_flag                              (* --name *)
            | short_flag                             (* -x *)
            | named                                  (* key=value *)
            | positional                             (* value *)
            ;

positional  = value ;
named       = IDENT , "=" , value ;

(* Flags *)
short_flag  = SHORT_FLAG ;                           (* -l, -la *)
long_flag   = LONG_FLAG ;                            (* --force *)
long_flag_with_value = LONG_FLAG , "=" , value ;     (* --message="x" *)

SHORT_FLAG  = "-" , ALPHA , { ALPHA | DIGIT } ;
LONG_FLAG   = "--" , IDENT ;
PLUS_FLAG   = "+" , ALPHA , { ALPHA | DIGIT } ;      (* +e for set +e *)
DOUBLE_DASH = "--" ;                                 (* end of flags marker *)

(* === Values === *)

value       = literal
            | var_ref
            | param_expansion
            | cmd_subst
            | string
            ;

literal     = INT
            | FLOAT
            | BOOL
            ;

cmd_subst   = "$(" , pipeline , ")" ;

(* Variable references *)
var_ref     = "${" , var_path , "}"                  (* ${VAR}, ${VAR.field}, ${VAR[0]} *)
            | "$" , IDENT                            (* $VAR - simple form *)
            | "$" , POSITIONAL                       (* $0-$9 *)
            | "$@"                                   (* all args *)
            | "$#"                                   (* arg count *)
            ;

var_path    = IDENT ;
POSITIONAL  = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;

(* Parameter expansion *)
param_expansion = "${" , IDENT , ":-" , value , "}"  (* ${VAR:-default} *)
                | "${#" , IDENT , "}"                (* ${#VAR} - string length *)
                ;

(* String literals *)
string      = DQSTRING                               (* "..." - interpolation *)
            | SQSTRING                               (* '...' - literal *)
            ;

(* Test expressions [[ ... ]] *)
test_expr_stmt = "[" , "[" , test_condition , "]" , "]" ;
test_condition = file_test | string_test | comparison ;
file_test   = FILE_OP , value ;                      (* [[ -f path ]] *)
string_test = STRING_OP , value ;                    (* [[ -z str ]] *)
comparison  = value , CMP_OP , value ;               (* [[ $X == "y" ]] *)

FILE_OP     = "-e" | "-f" | "-d" | "-r" | "-w" | "-x" ;
STRING_OP   = "-z" | "-n" ;
CMP_OP      = "==" | "!=" | "-gt" | "-lt" | "-ge" | "-le" | "=~" | "!~" ;

(* === Redirects === *)

redirect    = redir_op , value ;
redir_op    = ">" | ">>" | "<" | "2>" | "&>" ;

(* === Conditions === *)
(*
 * Shell-compatible conditions: commands whose exit codes determine truthiness.
 * - `if true; then` → runs `true` builtin (exit 0 = truthy)
 * - `if grep -q pattern file; then` → runs command, checks exit code
 * - `if cmd-a && cmd-b; then` → runs cmd-a, if exit 0, runs cmd-b
 * - For comparisons, use [[ ]]: `if [[ $X == 5 ]]; then`
 *
 * Precedence (highest to lowest):
 *   1. && (logical and) - short-circuit
 *   2. || (logical or) - short-circuit
 *
 * So: true || false && false  parses as  true || (false && false)
 *)
condition   = or_expr ;
or_expr     = and_expr , { "||" , and_expr } ;
and_expr    = base_cond , { "&&" , base_cond } ;
base_cond   = test_expr_stmt                         (* [[ -f path ]], [[ $X == 5 ]] *)
            | command                                 (* true, false, grep -q pattern, etc. *)
            ;

(* === Tokens (Lexer) === *)

(* Keywords - recognized at statement start *)
LOCAL       = "local" ;
TOOL        = "tool" ;
IF          = "if" ;
THEN        = "then" ;
ELIF        = "elif" ;
ELSE        = "else" ;
FI          = "fi" ;
FOR         = "for" ;
IN          = "in" ;
DO          = "do" ;
DONE        = "done" ;
WHILE       = "while" ;
BREAK       = "break" ;
CONTINUE    = "continue" ;
RETURN      = "return" ;
EXIT        = "exit" ;
SET         = "set" ;

(* Not keywords - parsed as command names *)
(* SOURCE and DOT are identifiers that command_parser accepts *)

(* Type keywords *)
TYPE        = "string" | "int" | "float" | "bool" ;

(* Literals *)
BOOL        = "true" | "false" ;
INT         = [ "-" ] , DIGIT , { DIGIT } ;
FLOAT       = INT , "." , DIGIT , { DIGIT } ;
DQSTRING    = '"' , { CHAR | ESCAPE | "$" , IDENT | "${" , var_path , "}" } , '"' ;
SQSTRING    = "'" , { CHAR - "'" } , "'" ;

(* Identifiers *)
IDENT       = ALPHA , { ALPHA | DIGIT | "-" | "_" } ;
ALPHA       = "a"-"z" | "A"-"Z" | "_" ;
DIGIT       = "0"-"9" ;

(* Variable references *)
VARREF      = "${" , { CHAR - "}" } , "}" ;
SIMPLEVAR   = "$" , IDENT ;

(* Escape sequences in double-quoted strings *)
CHAR        = (* any char except " \ $ *) ;
ESCAPE      = "\" , ( '"' | "\" | "n" | "t" | "r" | "$" | "u" , HEX , HEX , HEX , HEX ) ;
HEX         = DIGIT | "a"-"f" | "A"-"F" ;

(* Whitespace and structure *)
NEWLINE     = "\n" | "\r\n" ;
COMMENT     = "#" , { (* any char except newline *) } ;
LINECONT    = "\" , [ " " | "\t" ] , NEWLINE ;       (* line continuation *)
```

---

## Ambiguity Analysis

### Resolved Ambiguity 1: `set` Command vs Assignment

```bash
set -e                 # command (flag follows)
set +e                 # command (plus-flag follows)
set                    # command (terminator follows)
set X = 5              # assignment (IDENT '=' follows)
```

**Resolution:** Parser uses 1-token lookahead after `set`:
- If flag (`-x`, `+x`) → parse as command
- If terminator (newline, `;`, `&&`, `||`, EOF) → parse as command with no args
- If IDENT followed by `=` → parse as assignment

### Resolved Ambiguity 2: Tool Body vs Brace in Arguments

```bash
tool foo { ... }           # after 'tool IDENT', brace = body
```

**Resolution:** `{` after `tool IDENT` starts a tool body.

### Resolved Ambiguity 3: Named Args vs Positional

```bash
cmd foo=bar            # named arg (no spaces around =)
cmd foo = bar          # ERROR: '=' is not a valid argument
```

**Resolution:** No spaces around `=` in named args. With spaces, `=` becomes a bare token which is an error.

### Resolved Ambiguity 5: Flags vs Negative Numbers

```bash
cmd -123               # negative number (digit after -)
cmd -l                 # short flag (letter after -)
cmd --foo              # long flag
```

**Resolution:** Lexer priority. `-` followed by digit → Int. `-` followed by letter → ShortFlag.

### Resolved Ambiguity 6: Keywords Are Reserved

```bash
if="value"             # ERROR: 'if' is a keyword, cannot be variable name
for item in ...        # for loop (for at statement start)
myif="value"           # OK: 'myif' is not a keyword
```

**Resolution:** Keywords are always keywords. They cannot be used as variable names. This is intentional to avoid ambiguity and keep the language predictable. Use different names.

---

## Exit Code: `$?`

After any command, `$?` contains the **integer exit code** (0-255):

```bash
some-command
echo $?                    # prints: 0 (success) or 1-255 (failure)

if [[ $? -eq 0 ]]; then
    echo "success"
fi
```

**Note:** Unlike the original design, `$?` is a simple integer, not a structured object. Use tool-specific output capture for structured results.

---

## Test Categories

### Category 1: Lexer Tests (Token Stream)

```
┌─────────────────────────────────────────────────────────────────┐
│ Input                    │ Expected Tokens                     │
├─────────────────────────────────────────────────────────────────┤
│ X=5                      │ IDENT(X) EQ INT(5)                  │
│ echo "hello"             │ IDENT(echo) DQSTRING(hello)         │
│ echo $X                  │ IDENT(echo) SIMPLEVAR(X)            │
│ echo ${X}                │ IDENT(echo) VARREF(${X})            │
│ cmd a=1 b="x"            │ IDENT(cmd) IDENT(a) EQ INT(1) ...   │
│ # comment                │ COMMENT                              │
│ ls -l                    │ IDENT(ls) SHORTFLAG(l)              │
│ git --force              │ IDENT(git) LONGFLAG(force)          │
│ 'literal $x'             │ SQSTRING(literal $x)                │
│ set -e                   │ SET SHORTFLAG(e)                    │
│ set +e                   │ SET PLUSFLAG(e)                     │
│ [[                       │ LBRACKET LBRACKET                   │
│ "unterminated            │ ERROR: unterminated string          │
└─────────────────────────────────────────────────────────────────┘
```

### Category 2: Parser Tests (AST Structure)

```
┌─────────────────────────────────────────────────────────────────┐
│ Production    │ Input              │ Expected AST              │
├─────────────────────────────────────────────────────────────────┤
│ assignment    │ X=5                │ Assign(X, Int(5))         │
│ assignment    │ X="hello"          │ Assign(X, Str(hello))     │
│ assignment    │ local Y=10         │ LocalAssign(Y, Int(10))   │
│ command       │ echo "hi"          │ Cmd(echo, [Str(hi)])      │
│ command       │ set -e             │ Cmd(set, [ShortFlag(e)])  │
│ command       │ true               │ Cmd(true, [])             │
│ pipeline      │ a | b | c          │ Pipe([Cmd(a),Cmd(b),...]) │
│ named_arg     │ foo=123            │ Named(foo, Int(123))      │
│ flag          │ -l                 │ ShortFlag(l)              │
│ flag          │ --force            │ LongFlag(force)           │
│ param_exp     │ ${X:-default}      │ VarWithDefault(X, "...")  │
│ var_length    │ ${#X}              │ VarLength(X)              │
│ while         │ while true; do...  │ While(Cmd(true), [...])   │
│ test          │ [[ -f x ]]         │ Test(FileTest(IsFile, x)) │
└─────────────────────────────────────────────────────────────────┘
```

### Category 3: Round-Trip Tests (Parse → Print → Parse)

Property: `parse(print(parse(input))) == parse(input)`

Generate random valid ASTs, pretty-print them, parse again, compare.

### Category 4: Evaluation Tests (Input → Output)

Golden file tests: script + expected stdout/stderr/exit code.

```
┌─────────────────────────────────────────────────────────────────┐
│ Script                           │ Expected                    │
├─────────────────────────────────────────────────────────────────┤
│ echo "hello"                     │ stdout: hello               │
│ X=5; echo $X                     │ stdout: 5                   │
│ echo $UNDEFINED                  │ stderr: undefined var       │
│ false && echo "no"               │ stdout: (empty)             │
│ false || echo "yes"              │ stdout: yes                 │
│ X=${X:-default}; echo $X         │ stdout: default             │
│ X="hello"; echo ${#X}            │ stdout: 5                   │
│ set -e; false; echo "not here"   │ exit: 1 (never echoes)      │
└─────────────────────────────────────────────────────────────────┘
```

### Category 5: Error Message Tests

```
┌─────────────────────────────────────────────────────────────────┐
│ Input                │ Expected Error Contains                  │
├─────────────────────────────────────────────────────────────────┤
│ echo ${X.y.z         │ "unterminated variable reference"        │
│ tool { }             │ "expected tool name after 'tool'"        │
│ cmd foo = bar        │ "unexpected '='"                         │
│ break 0              │ "break level must be positive"           │
│ [[ ]]                │ "empty test expression"                  │
└─────────────────────────────────────────────────────────────────┘
```

### Category 6: Fuzz Tests

Throw random bytes at the parser. It should:
- Never panic
- Never hang
- Always return `Ok(AST)` or `Err(ParseError)`

---

## Flag Arguments

**Status:** Implemented

### Token Types

```ebnf
SHORT_FLAG  = "-" , ALPHA , { ALPHA | DIGIT } ;   (* -l, -la, -vvv *)
LONG_FLAG   = "--" , IDENT ;                       (* --force, --message *)
PLUS_FLAG   = "+" , ALPHA , { ALPHA | DIGIT } ;   (* +e, +x - for set +e *)
DOUBLE_DASH = "--" ;                               (* end of flags marker *)
```

### Examples

```bash
ls -l                    # SHORT_FLAG(l)
ls -la                   # SHORT_FLAG(la) - combined
git commit -m "msg"      # SHORT_FLAG(m), DQSTRING(msg)
git push --force         # LONG_FLAG(force)
git commit --message="x" # LONG_FLAG(message), EQ, DQSTRING(x)
set -e                   # SET, SHORT_FLAG(e)
set +e                   # SET, PLUS_FLAG(e)
```

### Edge Cases

| Input | Interpretation |
|-------|----------------|
| `--` | End of flags marker (everything after is positional) |
| `-` | Single dash - treat as positional (often means stdin) |
| `---` | Error: invalid flag syntax |
| `-123` | Negative integer, not a flag |
| `--foo-bar` | Long flag "foo-bar" |
| `+e` | Plus flag (only valid after `set`) |

---

## Line Continuation

A backslash at end of line continues the statement:

```bash
echo "this is a very long" \
     "command that spans" \
     "multiple lines"
```

The lexer produces a `LINECONT` token which is skipped, merging the lines.

