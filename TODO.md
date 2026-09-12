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

Measured by running the linter over Watertight's codebase

### Silently costs a rule rather than a statement

- **`CAST(x AS C(<expr>))`** costs the whole `SELECT` its parse, so the query's destination, joins and WHERE go unchecked. The only whole-file-scale gap left: `TypeSpec` reads the width as a `NumberLiteral`, and the widths come from the schema at runtime.
- **`DO FORM <a-b>`** reads the name as far as the hyphen, so the statement looks read and names the wrong form.

### Others

Every measured one is read now: `@ <row>,<col>` with no clause (147 uses), the console commands `EJECT`, `RETRY` and `SHOW GETS` (with `SHOW GET <var>`, whose operand is a read the symbol table was losing), `AS <type>` on `PRIVATE` and `PUBLIC`, `DELETE RECORD n`, `MD (<expr>)`, `MODIFY COMMAND (<expr>)`, `FLUSH IN`, `SET ORDER TO <expr>`, `IF ... THEN` and `DIMEN`. `ACTIVATE SCREEN`, `READ EVENTS`, `CANCEL`, `ADD OBJECT`, `SET RELATION OFF INTO` and the `AS` on a method return were already read when the list was written.

What is left in the ledger is unmeasured, and each item costs one statement: transactions (`BEGIN`/`END TRANSACTION`, `ROLLBACK`), the database container (`CREATE`/`OPEN DATABASE`, `CREATE CONNECTION`, `FREE`/`REMOVE TABLE`, and the `DELETE DATABASE`/`VIEW`/`CONNECTION` partials), console input (`INPUT`, `ACCEPT`), `READ CYCLE` and `@ ... EDIT`. Transactions are the only one with a rule waiting behind them. All are one-liners in `test-files/diagnostics/still-unsupported.prg`.

## Cleanup

- **e2e in CI** — `bun run e2e` is the only thing exercising the LSP over the wire, and it passes again as of this pass. It downloads VS Code and needs a display, so it would need `xvfb-run` and would be the flakiest job in the file. The case for a separate, non-blocking job: the suite was red for two releases, on two counts, and nobody knew. Two things to fix on the way: the runner exits 0 when Mocha finds no test files, and the quick fixes, Outline and folding have no e2e coverage yet.

## Practices

- Count a rule's hits over the corpus before writing it. The `=` rule: 42 hits, ~2 real. `unused-local`: six for six.
- Verify a grammar change by asking the parser what it returns, not by reading the grammar. Find the shape with a script, parse a sample, print the field, compare, then leave the comparison behind in `run-parse-tests.js`. Reading finds only what you're already looking for — a sweep verified by reading missed fifteen misparses, four in constructs a careful reader had signed off.
- Show each new test a broken version of what it guards and confirm it fails. The keyword-boundary probe passed against a grammar with the boundary deliberately removed.
- The line a parse error is reported on is never the line at fault. PEG rejects a block wholesale when anything inside it fails, so the opener falls through to the unsupported fallback and the error surfaces on an orphaned `ELSE` or `ENDIF` that can be hundreds of lines below. Find the culprit by building the block tree textually and descending to the smallest block that fails on its own, then confirm the construct in isolation. Bisecting on "does the prefix parse" does not work: the predicate is not monotonic, because every prefix ending mid-block fails too.
- A new gap fixture needs the control as well as the case: the fixture must fail, *and* the same code with only the named construct respelled must parse clean. Two of the gaps recorded in SEE-ALSO were found by the control half rather than the case half.
