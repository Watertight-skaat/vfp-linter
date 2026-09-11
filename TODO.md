# TODO

## Editor

- **Go to definition and hover** — a `DO x` or `x()` whose routine is in the same file can jump to it, and hovering a variable can show its declaration and type. Both come off the symbol table; cross-file needs a workspace index and a `SET PROCEDURE` reading, which is a larger step.
- **Misc "Quick fixes"** — `empty-branch` (remove the branch), `try-without-catch` (add `CATCH TO loErr`), `select-without-into` (append `INTO CURSOR`, naming it after the first table). Same shape as the three that exist: a title and the edits, carried on the diagnostic.

## Rules

- **Work-area handling** — three rules on one scope record: `USE` without `IN` clobbers the current work area; a `SELECT` inside `SCAN … ENDSCAN` that isn't restored corrupts the record pointer mid-loop; `LOCATE` whose result is used without checking `FOUND()` operates on the wrong record. The scope record carries the events in source order with `targeted` marking the `IN` form, which is the distinction all three turn on.
- **Cursor written to without READWRITE / NOFILTER** — `INTO CURSOR` can yield a filtered view over the base table rather than a real cursor; modifying it then misbehaves in ways that are painful to trace. The grammar parses both keywords and discards them, so they need keeping on the destination first.
- **Hungarian notation vs. actual use** — opt-in, off by default. VFP convention encodes scope (`l`/`t`/`g`/`p`) then type (`c n l d t o a`). The valuable check is not "does the name match a regex" but does the prefix match what's assigned — `lcCount = 0` declares a character variable and stores a number. The symbol table holds the declared type and every write site; the literal's type comes free from the node.
- **String `=` under SET EXACT OFF** — opt-in, off by default, or drop it. VFP's default makes `=` a prefix comparison that stops at the end of the right-hand operand, and it never errors. Run unsuppressed over the corpus it fires 42 times, roughly two of them real, so it needs four suppressions before it's shippable: skip fixed-width function returns (`TYPE(x) = "C"`, 13 hits), skip single-character literals (`m.cType = "M"`, 11), treat SQL separately (governed by `SET ANSI`, not `SET EXACT`, 11), and skip xbase scope clauses (`LOCATE FOR c = "P"`, 3). A rule that needs that many heuristics to be quiet will be turned off by every user who meets it. Now that `foxpro.rules` exists, "off by default" costs nothing.

## Grammar coverage

Measured by running the linter over all 1747 `.prg` files in `W:\DevStaging`: 47 files fail to parse outright and 5525 statements report `unsupported-syntax`. Every item below is pinned by a fixture, so its expectation file changes the day it is fixed. The full findings are in `test-files/watertight/SEE-ALSO.md`.

### Costs the whole file

These are worth an order of magnitude more than anything under them: a file that does not parse is a file where no rule runs at all, and the reported error is never on the offending line — PEG rejects a block wholesale when anything inside it fails, so the error surfaces on an orphaned `ELSE` or `ENDIF` far below. Thirteen constructs account for 43 of the 47 failures; they are listed in section 5 of SEE-ALSO with a fixture each in `test-files/watertight/diagnostics/gap-*.prg`. In rough order of what they cost:

- **A leading-dot member reference inside an expression** — `IF .ChartsCount > 1`, `CASE .Mode = 1`. On its own it causes more whole-file failures than everything else put together. The dot is already read where a statement *starts* with it; this is the same reference one level in.
- **`PARAMETERS()` the function**, which the declaration keyword currently wins. The only way the legacy code defaults an optional argument.
- **`SELECT()` the function**, same shape and about as expensive: saving and restoring the current work area around a lookup is the most repeated idiom in the source. `STORE SELECT(0) TO m.nArea` does parse, which is what makes the gap hard to see by reading.
- **`PROTECTED` / `HIDDEN` before `PROCEDURE` or `FUNCTION`**, which costs the whole `DEFINE CLASS`.
- **A second `CATCH`** in one `TRY`, which is the shape of every retry loop.
- Then `WITH .Member`, `LOOP`/`CLASS` as plain names, `DO CASE <expr>`, a second `OTHERWISE`, nested `#IF`, `DEFINE CLASS` with no `AS`, and `COPY TO ... NEXT n`.
- **A `#IF` fence that does not nest with the block structure around it** is the odd one out: it does not fail the parse, it reports `unterminated-block` at error severity against an `IF` that is terminated. A false error on correct code is worse than a gap that admits it.

### Silently costs a rule rather than a statement

- **A `WITH` member assignment inside a nested block** — `.Width = 400` under an `IF` inside a `WITH`. Reported as unsupported rather than misparsed, but form code conditions most of its property writes, so the symbol table sees a fraction of what a `WITH` block writes.
- **`BROWSE NORMAL` reads as `NORM` plus a leftover `AL`** — `"NORM"i` has no word boundary. The same class of bug `run-keyword-tests.js` exists to catch, one level in, and a check for it now lives there recording today's behaviour.
- **`CAST(x AS C(<expr>))`** costs the whole `SELECT` its parse, so the query's destination, joins and WHERE go unchecked.
- **`DO FORM <a-b>`** reads the name as far as the hyphen, so the statement looks read and names the wrong form.

### Announces itself, and is only worth the volume

Ordered by measured uses. `WAIT CLEAR` alone is 426 uses across 185 files, which makes it the most common unparsed statement in the source by a wide margin; `@ <row>,<col>` with no clause is 147. After those come the console commands (`ACTIVATE SCREEN`, `EJECT`, `READ EVENTS`, `RETRY`, `CANCEL`, `SHOW GETS`), `AS <type>` on `PRIVATE`/`PUBLIC`/a method return, `DELETE RECORD n`, `ADD OBJECT` in a class, `MD (<expr>)`, the clauses whose operand is an expression (`FLUSH IN`, `SET RELATION OFF INTO`, `SET ORDER TO <expr>`), `IF ... THEN`, and `DIMEN`. All are one-liners in `test-files/diagnostics/still-unsupported.prg`.

### Older items, still open

- **UNIQUE / FOREIGN KEY after the column list in `CREATE TABLE`** — column-level `UNIQUE`, `CHECK` and `REFERENCES` are read; a constraint written after the column list is not, and it costs the whole `CREATE TABLE` its parse. A second `ADD COLUMN` on `ALTER TABLE` is the same shape, 91 uses.
- **Pre-SQL data commands** — `TOTAL`, `JOIN WITH`, `UPDATE ON`, `COPY STRUCTURE`, `DELETE TAG`, `BLANK`. Each names a table or a variable, so they carry operands a rule would want.
- **`STORE 0 TO a[1], b[2]`** — multiple targets where one is subscripted. Currently announces itself rather than silently dropping the subscript, which is what it used to do.
- **Memory-variable and debugging commands** — `SAVE TO` / `RESTORE FROM`, `PRIVATE ALL EXCEPT`, `ASSERT`, `PLAY MACRO`.
- **Screen and menu commands** — `DEFINE WINDOW` / `BAR` / `MENU`, `ACTIVATE WINDOW`, `ON SELECTION`. A 30-year-old application carries a lot of them, but not one touches data or a variable, so no rule loses anything. Lowest value here.

Two found by a sweep that are defects in rules that already exist rather than missing ones:

- **`TOTAL TO totals ON custid`** — the documented argument order. `TotalStatement` reads only the reverse, `TOTAL ON key TO file`, so the canonical spelling falls to the catch-all. Accepting either order is the fix.
- **Three `SET`s whose argument runs past what the setting reader claims** — `SET TOPIC ID TO 5`, `SET NOTIFY CURSOR OFF` and `SET WINDOW OF MEMO notes TO myform`, each leaving the tail behind. (partial)

## Cleanup

- **e2e in CI** — `bun run e2e` is the only thing exercising the LSP over the wire, and it passes again as of this pass. It downloads VS Code and needs a display, so it would need `xvfb-run` and would be the flakiest job in the file. The case for a separate, non-blocking job: the suite was red for two releases, on two counts, and nobody knew. Two things to fix on the way: the runner exits 0 when Mocha finds no test files, and the quick fixes, Outline and folding have no e2e coverage yet.

## Practices

- Count a rule's hits over the corpus before writing it. The `=` rule: 42 hits, ~2 real. `unused-local`: six for six.
- Verify a grammar change by asking the parser what it returns, not by reading the grammar. Find the shape with a script, parse a sample, print the field, compare, then leave the comparison behind in `run-parse-tests.js`. Reading finds only what you're already looking for — a sweep verified by reading missed fifteen misparses, four in constructs a careful reader had signed off.
- Show each new test a broken version of what it guards and confirm it fails. The keyword-boundary probe passed against a grammar with the boundary deliberately removed.
- The line a parse error is reported on is never the line at fault. PEG rejects a block wholesale when anything inside it fails, so the opener falls through to the unsupported fallback and the error surfaces on an orphaned `ELSE` or `ENDIF` that can be hundreds of lines below. Find the culprit by building the block tree textually and descending to the smallest block that fails on its own, then confirm the construct in isolation. Bisecting on "does the prefix parse" does not work: the predicate is not monotonic, because every prefix ending mid-block fails too.
- A new gap fixture needs the control as well as the case: the fixture must fail, *and* the same code with only the named construct respelled must parse clean. Two of the gaps recorded in SEE-ALSO were found by the control half rather than the case half.
