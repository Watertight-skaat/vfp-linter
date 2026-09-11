# TODO

## Rules

- **Work-area handling** — three rules on one scope record: `USE` without `IN` clobbers the current work area; a `SELECT` inside `SCAN … ENDSCAN` that isn't restored corrupts the record pointer mid-loop; `LOCATE` whose result is used without checking `FOUND()` operates on the wrong record. The scope record carries the events in source order with `targeted` marking the `IN` form, which is the distinction all three turn on.
- **`duplicate-case` should skip a constant `.F.`** — `CASE .f.` is how a branch is switched off without deleting it, and it recurs across the source. Skipping conditions that are a constant `.F.` is a single test in an existing rule.
- **Hungarian notation vs. actual use** — VFP convention encodes scope (`l`/`t`/`g`/`p`) then type (`c n l d t o a`). The valuable check is not "does the name match a regex" but does the prefix match what's assigned — `lcCount = 0` declares a character variable and stores a number. The symbol table holds the declared type and every write site; the literal's type comes free from the node. Also most of what the `=` rule needs below.
- **String `=` under SET EXACT OFF** — VFP's default makes `=` a prefix comparison that stops at the end of the right-hand operand, and it never errors. Run unsuppressed over the corpus it fires 42 times, roughly two of them real, so it needs four suppressions before it's shippable: skip fixed-width function returns (`TYPE(x) = "C"`, 13 hits), skip single-character literals (`m.cType = "M"`, 11), treat SQL separately (governed by `SET ANSI`, not `SET EXACT`, 11), and skip xbase scope clauses (`LOCATE FOR c = "P"`, 3).
- **Cursor written to without READWRITE / NOFILTER** — `INTO CURSOR` can yield a filtered view over the base table rather than a real cursor; modifying it then misbehaves in ways that are painful to trace. The grammar parses both keywords and discards them, so they need keeping on the destination first.

## Grammar coverage

These announce themselves as `unsupported-syntax` rather than producing a wrong tree, so none of them block a rule.

- **UNIQUE / FOREIGN KEY after the column list in `CREATE TABLE`** — column-level `UNIQUE`, `CHECK` and `REFERENCES` are read; a constraint written after the column list is not, and it costs the whole `CREATE TABLE` its parse rather than one clause. The only gap that costs more than its own statement.
- **Pre-SQL data commands** — `TOTAL`, `JOIN WITH`, `UPDATE ON`, `COPY STRUCTURE`, `DELETE TAG`, `BLANK`. Each names a table or a variable, so they carry operands a rule would want.
- **`STORE 0 TO a[1], b[2]`** — multiple targets where one is subscripted. Currently announces itself rather than silently dropping the subscript, which is what it used to do.
- **Memory-variable and debugging commands** — `SAVE TO` / `RESTORE FROM`, `PRIVATE ALL EXCEPT`, `ASSERT`, `PLAY MACRO`.
- **Screen and menu commands** — `DEFINE WINDOW` / `BAR` / `MENU`, `ACTIVATE WINDOW`, `ON SELECTION`. A 30-year-old application carries a lot of them, but not one touches data or a variable, so no rule loses anything. Lowest value here.

## Project

- **Decide whether e2e belongs in CI** — `bun run e2e` is the only thing exercising the LSP over the wire and is deliberately not in the workflow: it downloads VS Code and needs a display, so it would need `xvfb-run` and would be the flakiest job in the file. Left out on the grounds that a flaky job is worse than no job — but that leaves e2e the one suite nothing runs automatically.

## Practices

- Count a rule's hits over the corpus before writing it. The `=` rule: 42 hits, ~2 real. `unused-local`: six for six.
- Verify a grammar change by asking the parser what it returns, not by reading the grammar. Find the shape with a script, parse a sample, print the field, compare. Reading finds only what you're already looking for — a sweep verified by reading missed fifteen misparses, four in constructs a careful reader had signed off.
- Show each new test a broken version of what it guards and confirm it fails. The keyword-boundary probe passed against a grammar with the boundary deliberately removed.
