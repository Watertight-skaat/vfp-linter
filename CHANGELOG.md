# Changelog

## Unreleased

### `unused-local`

The other half of `implicit-private`. That rule reports a name nothing declared; this one reports a
declaration nothing uses — `reads` and `writes` both empty in the symbol table. `LOCAL` only: `PUBLIC`
and `PRIVATE` exist to be seen by the routines you call, so silence in the declaring routine says
nothing about them, and an unused parameter is usually a signature the caller still passes.

Measured before it shipped, the way the roadmap's `=` rule taught: across 93 fixtures it fires six
times and every one is real. One of them is the shape the rule exists for — `form-launch.prg` declares
`oCustomer` and the code below uses `oCustomerForm`, so the pair now reports the dead declaration and
the accidental PRIVATE at the same time.

### Grammar coverage

Every construct on the roadmap's measured list now parses:

- `TEXT ... ADDITIVE` written anywhere in the option list, not only next to the variable. This was the
  one gap that cost a whole **file** its parse rather than one statement
- `SCAN ... WHILE ... FOR ...` in either order, which had been reported as an unterminated `FOR` block
  that was never opened — a false positive at error severity
- `SCATTER` and `GATHER` in every form, which is how a record becomes an object
- a keyword after a dot: `.To`, `.From`, `.Class`, `.Select`. `.AND.`, `.T.` and `.NULL.` are still
  told apart, by the closing dot
- `CATCH TO m.ErrObj`, the house spelling of `CATCH`
- `PARAM`, `PARAMETER` and `LPARAMETER`, which declare a routine's inputs
- a parenthesised alias wherever an alias is expected: `USE IN (m.cAlias)`, `SET ORDER TO (m.cTag)`,
  `GO TOP IN (m.cAlias)`. This unblocks the work-area rules
- `FLUSH`, `REINDEX`, `MD`/`RD`/`CD`, `COUNT`, `AVERAGE`, `CONTINUE`, `NODEFAULT`, `PUSH`/`POP KEY`,
  `EXTERNAL`, `MODIFY`, `ALTER TABLE`, `RUN`, `SET <x> TO` with no argument, and `WAIT WINDOW` with
  its flags after the message rather than before it

### Silent misparses

The previous round recorded none of these left. Sweeping the grammar again for a labelled group read
as if it were its own sequence found **thirteen** more, each of which parsed and then handed back the
wrong value — so no fixture could have reported any of them:

- `CATCH TO loErr` returned `"E"`, the third character of the name; `CATCH ... WHEN` returned nothing
- `ZAP IN ord` and `UNLOCK ... IN ord` returned `"d"`; `SKIP ... IN` and `UNLOCK RECORD` returned nothing
- `AS ... OF <library>` on `LOCAL` and on `LOCAL ARRAY`
- `COLLATE` on a column definition, on the `UNIQUE` constraint and on the `FOREIGN KEY` constraint,
  and `TAG` on a column's `REFERENCES` and on the `FOREIGN KEY`'s

Two more of other kinds, found while fixing those:

- **`USE IN cust`**, the ordinary way to close a work area, read as *opening* a table named `IN`,
  because the target pattern was greedy enough to swallow the option keyword after it. Every
  work-area event the symbol table recorded for one was wrong, and `USE IN <area>` was being modelled
  as closing the current area rather than that one
- `PrintStatement.argument` duplicated `arguments[0]`, so every variable read inside a `?` statement
  was counted twice. Nothing read the field; it is gone, like `returnExpression` before it

Fifteen in total, none of which any diagnostics fixture could see.

### The symbol table sees six more things

Each was a reference that reached it as nothing at all, because the statement either did not parse or
parsed with the name thrown away: `SCATTER NAME` and `GATHER NAME`, `CATCH TO`, `TEXT TO`,
`DO FORM ... NAME` and `... TO`, `USE (expr)`/`ALIAS (expr)`, and `&lcCommand` — which is a read of
`lcCommand`, and without it any macro-driven local looked unused.

### Tests

`run-keyword-tests.js` is new: it re-derives all 302 keyword literals from the grammar and probes each
as an identifier prefix in three statement shapes, because none of FoxPro's command words is reserved
and a rule without a word boundary silently beats the assignment below it. Scope assertions went from
42 to 55, fixtures from 92 to 93, and the `unsupported-syntax` ledger was rewritten to hold only
constructs that genuinely still do not parse.

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
