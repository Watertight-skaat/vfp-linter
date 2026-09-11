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

These announce themselves as `unsupported-syntax` rather than producing a wrong tree, so none of them block a rule; where one costs more than its own statement, or misparses instead, the bullet says so. Measured by probing the parser, not by reading the grammar; `test-files/diagnostics/still-unsupported.prg` holds the same list as a fixture, so implementing one makes the ledger shorter.

- **`DEFINE CLASS` member declarations** — `PROTECTED`, `HIDDEN`, `IMPLEMENTS` and `ADD OBJECT`. The largest group left. On a property each costs only its own line and the class around it still reads, but `PROTECTED PROCEDURE Foo` leaves the whole class unreadable, so every method in it leaves the outline and the symbol table together. That makes it the one to do first: it is the only gap left that costs more than its own statement.
- **The output commands** — `??`, which prints without the leading newline, and a `\` or `\\` line, which is TEXTMERGE output written one line at a time.
- **`APPEND MEMO` / `COPY MEMO`** — a memo field read from and written to a text file. These sit beside `APPEND FROM` and `COPY TO`, which are read.
- **`ON PAD` / `ON BAR`** — the menu handlers that open a submenu. `ON SELECTION`, which runs a command, is read; these two are not.
- **`SET` commands whose argument is a file path, or that carry a second clause of their own** — `SET DEFAULT TO c:\temp`, `SET PRINTER TO FILE x.txt`, `SET TEXTMERGE ON DELIMITERS TO "<<", ">>"`. Each leaves a `SetCommand` behind and only the tail is lost. The path case is the expression reader meeting a bare Windows path; a file name without a drive letter, `SET HELP TO x.hlp`, parses but reads as member access.
- **A quoted class library in an `AS ... OF` clause** — `LOCAL loX AS Poster OF "poster.vcx"`. The bare name reads, so the declaration is already in the symbol table and only the library is lost.
- **The remainder, each costing only its own statement** — `RETURN TO MASTER`, `CANCEL`, `READ EVENTS`, `COMPILE`, `BUILD APP`.

## Cleanup

- **e2e in CI** — `bun run e2e` is the only thing exercising the LSP over the wire, and it passes again as of this pass. It downloads VS Code and needs a display, so it would need `xvfb-run` and would be the flakiest job in the file. The case for a separate, non-blocking job: the suite was red for two releases, on two counts, and nobody knew. Two things to fix on the way: the runner exits 0 when Mocha finds no test files, and the quick fixes, Outline and folding have no e2e coverage yet.

## Practices

- Count a rule's hits over the corpus before writing it. The `=` rule: 42 hits, ~2 real. `unused-local`: six for six.
- Verify a grammar change by asking the parser what it returns, not by reading the grammar. Find the shape with a script, parse a sample, print the field, compare, then leave the comparison behind in `run-parse-tests.js`. Reading finds only what you're already looking for — a sweep verified by reading missed fifteen misparses, four in constructs a careful reader had signed off.
- Show each new test a broken version of what it guards and confirm it fails. The keyword-boundary probe passed against a grammar with the boundary deliberately removed.
- A fixture that passes can still be misparsing. `startup-settings.prg` passed the whole time its line 5 was wrong; the e2e suite was red for two releases with nothing running it. Coverage that nothing executes is a claim, not a check.
