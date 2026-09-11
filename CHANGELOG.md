# Changelog

## 1.1.0

The release that stops the linter crying wolf, and starts it finding real bugs.

### Unsupported syntax is no longer reported as wrong

The grammar does not cover all of FoxPro, and valid code that reached the catch-all rule used to get a
red squiggle. That is now advisory by default and configurable through the new
`foxpro.unsupportedSyntaxSeverity` setting (`error`, `warning`, `information`, `hint`, `off`).

Two cases are never quieted with it, because both are genuinely wrong: a syntax error the parser
throws on, and a block whose terminator is missing. The second needed its own diagnostic
(`unterminated-block`) because the catch-all swallows the opening line of an unterminated `IF`, `FOR`,
`TRY`, `WITH`, `DEFINE CLASS` or `TEXT` instead of failing the parse — so before this release, a
missing `ENDIF` was only reported by accident, and downgrading the catch-all alone would have stopped
reporting it at all.

### Ten new rules

- `missing-memvar-prefix` — a variable referenced without `m.` whose name the file also uses as a field
- `unreachable-code` — a statement after `RETURN` / `EXIT` / `LOOP` in the same block
- `duplicate-case` — a `CASE` condition identical to an earlier one in the same `DO CASE`
- `private-all` — `PRIVATE ALL`, which hides every variable of the caller
- `unlinked-tables` — tables in `FROM` with nothing relating them: a Cartesian product
- `select-without-into` — a query with no `INTO` or `TO`, which browses its whole result at run time
- `try-without-catch` — a `TRY` with neither `CATCH` nor `FINALLY`
- `empty-branch` — an `IF`, `ELSE`, `CASE` or `OTHERWISE` with no statements
- `unterminated-block` and `syntax-error`, above

Every diagnostic now carries a code, so any of them can be filtered from the Problems panel.

### Much more of the language parses

`TEXT ... ENDTEXT`, with the body kept as raw text rather than parsed as code. The `ON ERROR`,
`ON ESCAPE`, `ON SHUTDOWN`, `ON READERROR`, `ON PAGE` and `ON KEY LABEL` handlers. `THROW` as a
statement. The SQL `CASE WHEN` expression. The `::` scope-resolution operator.
`@ ... SAY` / `GET` / `TO` / `CLEAR`. And `CLEAR`, `CLOSE`, `RELEASE`, `PACK`, `SEEK`, `SUSPEND`,
`RESUME`, `KEYBOARD`, `LIST`, `DISPLAY`, `REPORT FORM`, `LABEL FORM` and `SORT`.

### Fixed

- **A `DO CASE` with more than one branch never parsed.** The second and later `CASE` lines were
  swallowed into the first branch, and a separate bug discarded every branch's contents outright, so
  nothing inside a `DO CASE` was ever checked.
- **Unrecognised commands were silently accepted rather than reported.** A bare word was treated as an
  expression, so `SEEK lcKey` parsed as two meaningless expressions instead of being reported as
  syntax the linter does not know.
- **Keywords matched the start of longer identifiers.** `DoSomething()` parsed as `DO` plus rubbish,
  `SELECTED = 1` as a `SELECT`, and `USEr`, `DELETEd`, `ZAPped`, `APPENDix`, `SUMmary` and `SKIPped`
  all misparsed.
- **`LOCAL` declarations, `COPY TO` and `APPEND FROM` lost their node type**, so no check could see
  them.
- `FOR ... NEXT`, `FUNCTION f() ... ENDFUNC` and `DEFINE CLASS ... OLEPUBLIC` now parse.
- `USE`, `CALCULATE` and `SUM` no longer discard their options; `SEEK ... ORDER TAG` keeps the tag.
- A `SCAN` body no longer swallows its own `ENDSCAN`, and an unterminated `DO WHILE` is reported.

### Performance

Diagnostics are debounced: a document has to stop changing for 300 ms before it is re-linted, with a
separate timer per document. Typing a burst of eleven edits used to queue eleven full parses; it now
queues one.

## 1.0.1

Initial published release.
