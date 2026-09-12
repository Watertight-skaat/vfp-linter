# TODO

## Editor

- **Go to definition and hover** — a `DO x` or `x()` whose routine is in the same file can jump to it, and hovering a variable can show its declaration and type. Both come off the symbol table; cross-file needs a workspace index and a `SET PROCEDURE` reading, which is a larger step.
- **Misc "Quick fixes"** — `empty-branch` (remove the branch), `try-without-catch` (add `CATCH TO loErr`), `select-without-into` (append `INTO CURSOR`, naming it after the first table). Same shape as the three that exist: a title and the edits, carried on the diagnostic.

## Rules

- **Work-area handling** — three rules on one scope record: `USE` without `IN` clobbers the current work area; a `SELECT` inside `SCAN … ENDSCAN` that isn't restored corrupts the record pointer mid-loop; `LOCATE` whose result is used without checking `FOUND()` operates on the wrong record. The scope record carries the events in source order with `targeted` marking the `IN` form, which is the distinction all three turn on.
- **A transaction that is never closed** — the rule that was waiting on the grammar. `BEGIN TRANSACTION` with no `END TRANSACTION` or `ROLLBACK` on the way out of the routine leaves the writes uncommitted and the locks held, and the buffering commands it wraps are already read. Count its hits over the corpus first: the frame is usually a few lines long, so the shape may not occur often enough to be worth a rule.
- **Cursor written to without READWRITE / NOFILTER** — `INTO CURSOR` can yield a filtered view over the base table rather than a real cursor; modifying it then misbehaves in ways that are painful to trace. The grammar parses both keywords and discards them, so they need keeping on the destination first.
- **Hungarian notation vs. actual use** — opt-in, off by default. VFP convention encodes scope (`l`/`t`/`g`/`p`) then type (`c n l d t o a`). The valuable check is not "does the name match a regex" but does the prefix match what's assigned — `lcCount = 0` declares a character variable and stores a number. The symbol table holds the declared type and every write site; the literal's type comes free from the node.
- **String `=` under SET EXACT OFF** — opt-in, off by default, or drop it. VFP's default makes `=` a prefix comparison that stops at the end of the right-hand operand, and it never errors. Run unsuppressed over the corpus it fires 42 times, roughly two of them real, so it needs four suppressions before it's shippable: skip fixed-width function returns (`TYPE(x) = "C"`, 13 hits), skip single-character literals (`m.cType = "M"`, 11), treat SQL separately (governed by `SET ANSI`, not `SET EXACT`, 11), and skip xbase scope clauses (`LOCATE FOR c = "P"`, 3). A rule that needs that many heuristics to be quiet will be turned off by every user who meets it. Now that `foxpro.rules` exists, "off by default" costs nothing.

## Grammar coverage

What is left in `test-files/diagnostics/still-unsupported.prg` is not statements to add but two ambiguities to resolve, which is why neither is a one-liner: `ON()` reporting the current handler in a file where `ON` is also a command word, and whitespace between an alias and its dotted field, which has to be told apart from the dot operators.

## Practices

- Count a rule's hits over the corpus before writing it. The `=` rule: 42 hits, ~2 real. `unused-local`: six for six.
- Verify a grammar change by asking the parser what it returns, not by reading the grammar. Find the shape with a script, parse a sample, print the field, compare, then leave the comparison behind in `run-parse-tests.ts`. Reading finds only what you're already looking for — a sweep verified by reading missed fifteen misparses, four in constructs a careful reader had signed off.
- Show each new test a broken version of what it guards and confirm it fails. The keyword-boundary probe passed against a grammar with the boundary deliberately removed.
- The line a parse error is reported on is never the line at fault. PEG rejects a block wholesale when anything inside it fails, so the opener falls through to the unsupported fallback and the error surfaces on an orphaned `ELSE` or `ENDIF` that can be hundreds of lines below. Find the culprit by building the block tree textually and descending to the smallest block that fails on its own, then confirm the construct in isolation. Bisecting on "does the prefix parse" does not work: the predicate is not monotonic, because every prefix ending mid-block fails too.
- A new gap fixture needs the control as well as the case: the fixture must fail, *and* the same code with only the named construct respelled must parse clean. Two of the gaps recorded in SEE-ALSO were found by the control half rather than the case half.
