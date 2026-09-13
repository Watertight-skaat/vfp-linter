# TODO List

Remove items once addressed.

## Editor

- **Hover over a variable** — it would be nice to see original declared type and other useful information
- **Misc "Quick fixes"** — `empty-branch` (remove the branch), `try-without-catch` (add `CATCH TO loErr`), `select-without-into` (append `INTO CURSOR`, naming it after the first table). Same shape as the three that exist: a title and the edits, carried on the diagnostic.

## Potential Rules

- **Work-area handling** — three rules on one scope record: `USE` without `IN` clobbers the current work area; a `SELECT` inside `SCAN … ENDSCAN` that isn't restored corrupts the record pointer mid-loop; `LOCATE` whose result is used without checking `FOUND()` operates on the wrong record. The scope record carries the events in source order with `targeted` marking the `IN` form, which is the distinction all three turn on.
- **Cursor written to without READWRITE / NOFILTER** — `INTO CURSOR` can yield a filtered view over the base table rather than a real cursor; modifying it then misbehaves in ways that are painful to trace. The grammar parses both keywords and discards them, so they need keeping on the destination first.
- **Hungarian notation vs. actual use** — opt-in, off by default. VFP convention encodes scope (`l`/`t`/`g`/`p`) then type (`c n l d t o a`). The valuable check is not "does the name match a regex" but does the prefix match what's assigned — `lcCount = 0` declares a character variable and stores a number. The symbol table holds the declared type and every write site; the literal's type comes free from the node.
- **String `=` under SET EXACT OFF** — opt-in, off by default, or drop it. VFP's default makes `=` a prefix comparison that stops at the end of the right-hand operand, and it never errors. Run unsuppressed over the corpus it fires 42 times, roughly two of them real, so it needs four suppressions before it's shippable: skip fixed-width function returns (`TYPE(x) = "C"`, 13 hits), skip single-character literals (`m.cType = "M"`, 11), treat SQL separately (governed by `SET ANSI`, not `SET EXACT`, 11), and skip xbase scope clauses (`LOCATE FOR c = "P"`, 3). A rule that needs that many heuristics to be quiet will be turned off by every user who meets it. Now that `foxpro.rules` exists, "off by default" costs nothing.

## Grammar coverage

Each of the below should have a `test-files/watertight/diagnostics/`. 


**Silent, so fix first:**

- **A string literal is allowed to cross a line break** (`gap-string-spans-lines.prg`) — FoxPro's tokenizer ends a literal at the newline; this one runs on, so a single stray quote swallows every line up to the next quote anywhere in the file. The swallowed lines produce *no diagnostic of any kind* — not unsupported-syntax, not a block error — so the linter quietly stops seeing code and every rule downstream reports on a file with a hole in it. This is the only failure in the set that a user cannot notice. It is also what cost `programs\app\WTMOBILEPROCESS.prg` its parse: a stray apostrophe after the `ENDFOR` at 3325 ran on for 78 lines and ate the header of the procedure below it, which is why the errors landed on `SaveInfoFields` and nothing in that procedure explained them — the one file `SEE-ALSO.md` had left unattributed. The fix is in the lexer: terminate a literal at end of line and report the unterminated quote where it opens.

**Cost a block**

- **Abbreviated keywords** (`gap-abbreviated-keywords.prg`) — FoxPro accepts any keyword cut to four characters, and the 2.75 generation writes `ENDI`, `ENDD`, `ENDC`, `DELE`, `ACTI`, `EXCLU`, `DESC` throughout. An abbreviated *terminator* is not recognised as one, so its block stays open and every later terminator closes the wrong thing. A dozen files in `programs\legacy275` die on a single `endi`.
- **`REPLACE` with a computed target** (`gap-replace-computed-field.prg`) — `REPLACE (expr) WITH ... IN (alias)` and `replace ;` with the field list on the next line. `REPLACE` is rejected, which leaves its `WITH` standing at the start of a statement where `WITH` opens a member scope: the linter then asks for an `ENDWITH`. Costs `TranNum.prg` and `qbgetcustomfields.prg`, both on the invoice path.
- **`#IF .F.` bodies are parsed** (`gap-preprocessor-false-fence.prg`) — the preprocessor never compiles them, and the code parks prose and unfinished work behind them. Any `if` in an English sentence opens a block; 72 findings on `mosesrules.prg` alone, none about code that runs. The fix is to evaluate a constant condition and skip the body.
- **A keyword as a `CASE` subject** (`gap-keyword-as-case-subject.prg`) — the 2.75 menus keep the operator's choice in a variable named `SELECT`. The first `CASE` is rejected, so the `DO CASE` has no branches and the `OTHERWISE` and `ENDCASE` are orphaned. `store select (0) to x` — the same call with a space before the parenthesis — fails where `SELECT(0)` parses.
- **`DELETE ... IN <alias> FOR <expr>`** (`gap-delete-in-alias.prg`) — plain `DELETE IN <alias>` parses; the `FOR` is read as a counted loop and swallows the routine.
- **Bare `USE` with a trailing `&&` comment** (`gap-bare-use-with-comment.prg`) — the comment is read as the file name, and an `if` among its words opens a block that never closes. The comment above a close is near-universal here.

**Announce themselves, but lose a declaration or invent a finding:**

- **`AS <dotted.Type>` in a declaration list** (`gap-typed-declaration-list.prg`) — `LOCAL loEx AS Exception` parses, `LOCAL loFSO AS Scripting.FileSystemObject` does not, and every name after it on the line goes unrecorded. One unread declaration becomes an `implicit-private` at every assignment: 51 of them for a single `loEx`.
- **The `?` parameter marker** (`gap-sql-parameter-marker.prg`) — the `WHERE` is rejected, so the `INTO CURSOR` below it is orphaned and `select-without-into` reports a query whose destination is written two lines down. A gap that only announced itself would be cheaper than one that makes a rule lie.
- **`&alias..field`** (`gap-macro-dot-field.prg`) — the macro terminator dot followed by a field. `websync.PRG` builds every query this way.
- **A table named by an expression** (`gap-computed-table-target.prg`) — `UPDATE (m.cTable) SET`, `ALTER TABLE (m.cTable)`, `CREATE CURSOR &cTable.`. `SELECT ... FROM (m.cTable)` already parses; the write statements do not.
- **`REGIONAL`** — one unread declaration, three `implicit-private` findings; 129 occurrences, all of the same line. Ledgered in `still-unsupported.prg`.

**One-liners, in `test-files/diagnostics/still-unsupported.prg`:** `#REGION` / `#ENDREGION` (754 occurrences, the single most common thing the grammar does not read, and trivial — they are folding markers with no semantics), `DOEVENTS`, `ERROR <n>`, and a bare `.Click` standing as a statement inside `WITH` (`.SetFocus()` parses because the call makes it an expression).

The two long-standing ambiguities in that ledger are unchanged and are still not one-liners: `ON()` reporting the current handler in a file where `ON` is also a command word, and whitespace between an alias and its dotted field, which has to be told apart from the dot operators.

**Not a gap:** `scripts\separateabranch.PRG` really is missing an `ENDCASE` — a `DO case` with three branches and an `OTHERWISE`, and the file ends after the `ENDFOR`. It is a hand-run maintenance script, so nothing would have caught it. That one belongs to Watertight rather than here.

**Still to settle:** `programs\app\TABLERULESINVTRAN.prg:118` and `:375` both read `USE proddet IN 0 order`, with `ORDER` carrying no tag. Every other `USE ... ORDER` in the tree names one, so it looks like a truncated line — but the file is compiled into the shipping exe, so VFP evidently accepts it, which makes it a grammar gap and not a defect. Worth confirming with `run_vfp.py` against a scratch table before deciding which list it belongs on; it costs two blocks either way.