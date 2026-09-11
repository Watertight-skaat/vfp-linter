# TODO

## Editor

- **Go to definition and hover** — a `DO x` or `x()` whose routine is in the same file can jump to it, and hovering a variable can show its declaration and type. Both come off the symbol table; cross-file needs a workspace index and a `SET PROCEDURE` reading, which is a larger step.
- **Quick fixes for the rest** — `empty-branch` (remove the branch), `try-without-catch` (add `CATCH TO loErr`), `select-without-into` (append `INTO CURSOR`, naming it after the first table). Same shape as the three that exist: a title and the edits, carried on the diagnostic.

## Rules

- **Work-area handling** — three rules on one scope record: `USE` without `IN` clobbers the current work area; a `SELECT` inside `SCAN … ENDSCAN` that isn't restored corrupts the record pointer mid-loop; `LOCATE` whose result is used without checking `FOUND()` operates on the wrong record. The scope record carries the events in source order with `targeted` marking the `IN` form, which is the distinction all three turn on.
- **Cursor written to without READWRITE / NOFILTER** — `INTO CURSOR` can yield a filtered view over the base table rather than a real cursor; modifying it then misbehaves in ways that are painful to trace. The grammar parses both keywords and discards them, so they need keeping on the destination first.
- **Hungarian notation vs. actual use** — opt-in, off by default. VFP convention encodes scope (`l`/`t`/`g`/`p`) then type (`c n l d t o a`). The valuable check is not "does the name match a regex" but does the prefix match what's assigned — `lcCount = 0` declares a character variable and stores a number. The symbol table holds the declared type and every write site; the literal's type comes free from the node.
- **String `=` under SET EXACT OFF** — opt-in, off by default, or drop it. VFP's default makes `=` a prefix comparison that stops at the end of the right-hand operand, and it never errors. Run unsuppressed over the corpus it fires 42 times, roughly two of them real, so it needs four suppressions before it's shippable: skip fixed-width function returns (`TYPE(x) = "C"`, 13 hits), skip single-character literals (`m.cType = "M"`, 11), treat SQL separately (governed by `SET ANSI`, not `SET EXACT`, 11), and skip xbase scope clauses (`LOCATE FOR c = "P"`, 3). A rule that needs that many heuristics to be quiet will be turned off by every user who meets it. Now that `foxpro.rules` exists, "off by default" costs nothing.

## Grammar coverage

These announce themselves as `unsupported-syntax` rather than producing a wrong tree, so none of them block a rule. Measured by probing the parser, not by reading the grammar; `test-files/diagnostics/still-unsupported.prg` holds the same list as a fixture, so implementing one makes the ledger shorter.

- **`SELECT()` as a function** — `lnArea = SELECT("customer")` falls to the catch-all because `SELECT` is in the `Keyword` list and `Identifier` refuses it. The lost assignment then makes `lnArea` an `unused-local`: a false positive on a very common idiom. The `"SELECT(0)"` special case inside `NumberLiteral` is a band-aid for exactly this, and is itself wrong, since it turns a function call into the literal zero.
- **Hex and scientific literals** — `x = 0x1F` reads as `x = 0` followed by an unknown statement `x1F`; `1E5` likewise. Both are valid VFP, and this one is a misparse rather than a gap.
- **`#IF … #ENDIF` bodies** — the whole block is kept as raw text, so code inside `#IF .T.` is invisible to the symbol table and every rule, and a nested `#IF` ends at the first `#ENDIF`.
- **Bare `TRUE` / `FALSE`** — accepted as boolean literals, which VFP does not have. A variable of that name silently vanishes from the symbol table.
- **`NOTE`** — the oldest comment form, reported as unsupported.
- **Dangling terminators** — a stray `ENDIF` throws, so the user loses every other diagnostic in the file until it is fixed. Absorbing it into a `syntax-error` diagnostic instead of throwing would keep the rest live while typing.
- **`SET` commands whose argument is a list or has a clause of its own** — `SET SKIP TO x INTO y`, `SET RELATION OFF INTO y`, `SET PROCEDURE TO a, b ADDITIVE`, `SET CLASSLIB TO x IN y ALIAS z`. The bare `SET x TO y` form reads them, so each leaves a `SetCommand` behind and only the tail is lost. The largest group left, and the one most likely to hold something a rule wants.
- **`BROWSE` options past the first** — the statement is recognised, so the work area is known; the field list is not.
- **`SAVE WINDOW` / `RESTORE WINDOW`** — window definitions written to and read back from a file. The screen commands proper are read now; these two sit beside `SAVE TO` and `RESTORE FROM` and are not.
- **The remainder, each costing only its own statement** — `INSERT BEFORE`, `FIND`, `COPY INDEXES`, `CREATE VIEW`, `DECLARE laArr[3]`, `WAIT ... TO`, `DEBUGOUT`. `DECLARE` of an array is the one of these that names a variable.

## Cleanup

- **e2e in CI** — `bun run e2e` is the only thing exercising the LSP over the wire, and it passes again as of this pass. It downloads VS Code and needs a display, so it would need `xvfb-run` and would be the flakiest job in the file. The case for a separate, non-blocking job: the suite was red for two releases, on two counts, and nobody knew. Two things to fix on the way: the runner exits 0 when Mocha finds no test files, and the quick fixes, Outline and folding have no e2e coverage yet.

## Practices

- Count a rule's hits over the corpus before writing it. The `=` rule: 42 hits, ~2 real. `unused-local`: six for six.
- Verify a grammar change by asking the parser what it returns, not by reading the grammar. Find the shape with a script, parse a sample, print the field, compare. Reading finds only what you're already looking for — a sweep verified by reading missed fifteen misparses, four in constructs a careful reader had signed off.
- Show each new test a broken version of what it guards and confirm it fails. The keyword-boundary probe passed against a grammar with the boundary deliberately removed.
- A fixture that passes can still be misparsing. `startup-settings.prg` passed the whole time its line 5 was wrong; the e2e suite was red for two releases with nothing running it. Coverage that nothing executes is a claim, not a check.
