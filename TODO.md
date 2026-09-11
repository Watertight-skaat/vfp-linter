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

Reported as `unsupported-syntax`. `test-files/diagnostics/still-unsupported.prg` holds the list, and everything on it was found by probing the parser rather than by reading the grammar.

- **`CREATE TRIGGER` / `DELETE TRIGGER`, and `VALIDATE DATABASE`** — the referential-integrity side of the database container. `DELETE TRIGGER` is partial: `DELETE` parses and `ON customer FOR INSERT` is what is lost.
- **`RENAME TABLE` and `RENAME CLASS`** — the container forms. The file form, `RENAME old.dbf TO new.dbf`, is read; these rename an object inside the container instead, and `TABLE` is the one that appears in real code.
- **`SHUTDOWN`** — ends the session, running `ON SHUTDOWN` first. `QUIT`, which does not, is read.
- **The Foxbase menu system** — `MENU BAR`, `MENU TO` and `READ MENU TO`, which predate `DEFINE POPUP` and still turn up in the oldest files. `MENU TO` puts the chosen bar number in a variable, so what is lost is a write the symbol table never sees.
- **`RELEASE MENU` / `RELEASE POPUP`** — partial, and the worst shape of the five: `RELEASE` reads as far as the word, then takes `MENU` for the name of a variable to release and leaves the real name behind. It is the only one here that misparses rather than reporting, so fixing it is worth more than the line count suggests.

Two things a sweep found that are not gaps but misparses, so nothing reports them:

- **`USE customer ORDER TAG custid`** — `OrderSpec` is only reachable through `USE ... ?`, so every word of the `ORDER` clause falls to `UseConnPart` and is read as a connection handle, the last one winning. The clause itself is already written; it needs adding to `UseOption` above the handle alternative.
- **`SET HELP TO x.hlp`** — parses, but reads the file as member access on a variable called `x`, which books a read of a name that does not exist. Same shape as the index-file fix, one level up in `SetCommand`'s argument list.

## Cleanup

- **e2e in CI** — `bun run e2e` is the only thing exercising the LSP over the wire, and it passes again as of this pass. It downloads VS Code and needs a display, so it would need `xvfb-run` and would be the flakiest job in the file. The case for a separate, non-blocking job: the suite was red for two releases, on two counts, and nobody knew. Two things to fix on the way: the runner exits 0 when Mocha finds no test files, and the quick fixes, Outline and folding have no e2e coverage yet.

## Practices

- Count a rule's hits over the corpus before writing it. The `=` rule: 42 hits, ~2 real. `unused-local`: six for six.
- Verify a grammar change by asking the parser what it returns, not by reading the grammar. Find the shape with a script, parse a sample, print the field, compare, then leave the comparison behind in `run-parse-tests.js`. Reading finds only what you're already looking for — a sweep verified by reading missed fifteen misparses, four in constructs a careful reader had signed off.
- Show each new test a broken version of what it guards and confirm it fails. The keyword-boundary probe passed against a grammar with the boundary deliberately removed.
- A fixture that passes can still be misparsing. `startup-settings.prg` passed the whole time its line 5 was wrong; the e2e suite was red for two releases with nothing running it. Coverage that nothing executes is a claim, not a check.
