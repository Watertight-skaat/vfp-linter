# Changelog

## 1.2.4

### `DEFINE CLASS` member declarations

`PROTECTED`, `HIDDEN`, `IMPLEMENTS` and `ADD OBJECT`. The one that mattered was the access word on a
method: `PROTECTED PROCEDURE Foo` left the whole class unreadable, so every method in it left the
Outline and the symbol table together. A method now carries `access`, the property form is its own
node, and `ADD OBJECT` keeps its class and its `WITH` pairs. The class scope and the Outline both book
the names all three declare, the way `cName = ""` at class-body level already was. Neither word is
reserved, so `protected = .T.` is still an assignment.

### The output commands

`??` writes at the cursor and `???` goes to the printer; `PrintStatement` carries which. A `\` or `\\`
line is TEXTMERGE output written one line at a time, kept verbatim because it is text rather than code.

### `SET` file paths and second clauses

`SET DEFAULT TO c:\temp` read as the name `c` and left `:\temp` to the catch-all, `SET PRINTER TO FILE
x.txt` lost the destination, and `SET TEXTMERGE ON DELIMITERS TO "<<", ">>"` stopped at the second
clause. `SetCommand` now carries `file` and `delimiters`, and a bare Windows path is read as a path.

### The rest of the old ledger

`APPEND MEMO` / `COPY MEMO`, `ON PAD` / `ON BAR`, `CANCEL`, `READ EVENTS`, `COMPILE`, `BUILD APP` and
`RETURN TO MASTER`, which used to report its own tail as unreachable code as well. `AS` and `OF` now
take a quoted name -- `LOCAL loX AS Poster OF "poster.vcx"` -- and `CANCEL` joins `RETURN`, `EXIT` and
`LOOP` as a statement nothing below it in the block can run after.

The ledger fixture that recorded these is now four lines long, each one measured by probing the
parser rather than read out of the grammar: `SAVE SCREEN` / `RESTORE SCREEN`, the `TO` clause of
`SET MARK OF`, and an index file named with its extension in an `OF` clause.

## 1.2.3

Grammar coverage: every gap the roadmap had measured is closed, two silent misparses found while
verifying them are fixed, and a stray block terminator no longer costs the file its parse.

### A dangling terminator no longer throws

A stray `ENDIF` used to make the parser give up on the whole file, so the user lost every other
diagnostic in it until the line was fixed -- and while typing, that line is usually the one being
written. A terminator with nothing open for it to close is now absorbed at file level and reported
as `syntax-error` in place, which is what it is, and everything below it is still checked. Twenty
thousand random token soups now parse without throwing.

### `SELECT()` as a function

`SELECT` is in the keyword list, so `lnArea = SELECT("customer")` fell to the catch-all and `lnArea`
was then reported as an unused local: a false positive on a very common idiom. The opening
parenthesis is what tells the function from the command. The `"SELECT(0)"` special case inside
`NumberLiteral` went with it -- it turned a function call into the literal zero.

### Hex and scientific literals

`x = 0x1F` read as `x = 0` followed by an unknown statement `x1F`, and `1E5` the same way. Both are
one token now, and both carry their real value.

### `#IF ... #ENDIF` bodies

The whole block was kept as raw text, so code inside `#IF .T.` was invisible to the symbol table and
to every rule, and a nested `#IF` ended at the first `#ENDIF`. The body is parsed, `#ELSE` and
`#ELIF` fill the branch below the way `IfStatement` does, `#IFDEF` and `#IFNDEF` are read, and the
condition stays raw because the preprocessor evaluates it against `#DEFINE` constants rather than
variables.

### Bare `TRUE` and `FALSE` are names again

VFP has `.T.` and `.F.` only. Read as boolean literals, a variable of either name vanished from the
symbol table.

### `NOTE`

The oldest comment form. It returns nothing rather than a node, because a comment is not a
statement, and it refuses the shapes a variable or an object of that name would take in command
position, so `note = x` and `note.caption = x` are untouched.

### `SET` argument lists and clauses

`SET PROCEDURE TO lib1, lib2 ADDITIVE`, `SET CLASSLIB TO x IN y ALIAS z`, `SET RELATION OFF INTO y`,
`SET SKIP TO x INTO y`. The argument is a list rather than one expression, and `IN`, `INTO`, `ALIAS`,
`ADDITIVE` and `ON`/`OFF` are read after it; `SetCommand` carries `arguments`, `inTarget`, `into` and
`alias` for them. Found while doing it: `SET TOPIC TO "x"` read as `SET TO` with a setting called
`PIC`, because the `TO` literal had no word boundary -- a valid tree that reported nothing.

### `BROWSE` options past the first

The whole documented option set is recognised, so the statement ends where it ends instead of
leaving everything after the first option to the catch-all.

### The rest of the ledger

`SAVE WINDOW` / `RESTORE WINDOW`, `INSERT [BLANK] [BEFORE]`, `FIND`, `COPY INDEXES`, `CREATE VIEW`,
`DECLARE laArr[3]`, `WAIT ... TO` and `DEBUGOUT`. `DECLARE` of an array is the older spelling of
`DIMENSION` and returns the same node, so it reaches the symbol table by the same path; `WAIT` is no
longer `WAIT WINDOW` only, and its `TO` clause books the variable it creates.

### A suite that asserts what the parser returns

`run-parse-tests.js` asserts the tree for each construct directly. A diagnostics fixture can only say
that nothing was reported, which a misparse satisfies as well as a correct parse -- both of the
misparses fixed above had been sitting behind a fully passing corpus.

## 1.2.2

The editor side: every rule gets a severity setting, any finding can be suppressed in place, three
rules fix themselves, and the file gets an Outline, folding and a language configuration. Underneath,
the rules are now a registry rather than a switch.

### Per-rule severities

`foxpro.rules` sets each rule to `error`, `warning`, `information`, `hint` or `off`; a rule not named
keeps its default. `foxpro.unsupportedSyntaxSeverity` still works and is marked deprecated, since it
is now the same setting for one rule. `syntax-error` and `unterminated-block` are locked at error.

### Suppression comments

`* vfp-lint-disable-next-line code`, `&& vfp-lint-disable-line code` on the statement itself, and
`* vfp-lint-disable code` ... `* vfp-lint-enable code` around a region. No code means every rule; a
reason can follow `--`. Every diagnostic offers *Suppress on this line* as a quick fix, which writes
the comment.

### Quick fixes

`implicit-private` adds the `LOCAL` at the top of the routine -- not above the first write, which is
often inside a loop where `LOCAL` would reset the value on every pass. `unused-local` removes the name
from its `LOCAL` line, or the line when it was alone. `missing-memvar-prefix` inserts the `m.`, and only
where the source at the reported position is the name itself, so a reference the grammar pinned to a
whole statement gets no fix rather than a wrong one.

### Outline, folding and language configuration

Procedures, functions, classes with their methods and properties, and `#DEFINE` constants appear in
the Outline and breadcrumbs. Every block folds, including a routine with no `ENDPROC`, each `CASE`,
and a `SELECT` written over several lines. The new `language-configuration.json` gives the language
`&&` comment toggling, bracket and quote pairing, and indentation after a block opener.

### A routine ends where the next one starts

`ENDPROC` is optional, and the grammar used to read every routine after an unterminated one as part of
its body, so a file of ten procedures parsed as one nested ten deep. A routine now stops at the next
`PROCEDURE`, `FUNCTION` or `DEFINE CLASS`. The symbol table's `parent` is now the file for every
file-level routine, and the routine's location now starts at its keyword rather than after its name.
The top-level statement list is also flattened like every block, which fixes `unreachable-code`
reporting a file-level `LOCAL` after `RETURN` at line 1 instead of where it is.

### The syntax-error diagnostic

A parse failure now goes through the same `lint()` path as everything else, so it carries the code
`syntax-error` and the same source as the other diagnostics; the harness used to fabricate it.
`duplicate-case` also now skips a constant `CASE .F.`, which is how a branch is switched off without
deleting it.

### The end-to-end suite

Was asserting that an unsupported statement is an error, which stopped being true when the severity
setting arrived, and its "clean" fixture looped over an undeclared `i`, which `implicit-private` has
rightly reported since it arrived; nothing ran the suite, so neither was noticed. It now asserts the
code and the advisory severity, the fixture declares its loop variable, and the runner clears
`ELECTRON_RUN_AS_NODE` so it can be started from VS Code's own terminal, where the downloaded
Code.exe used to inherit that variable and reject every launch flag.

---

Grammar coverage: the five gaps the roadmap had measured are closed, and one silent misparse found
while verifying them is fixed.

### Table-level constraints in `CREATE TABLE`

`UNIQUE`, `PRIMARY KEY`, `FOREIGN KEY` and `CHECK` written after the column list now parse into
`constraints`. This was the only gap that cost more than its own statement: a constraint opens with
words a column definition also swallows — `UNIQUE stnum TAG stnum` reads as a column named `UNIQUE` of
type `stnum` — and the column then stopped mid-clause, taking the whole `CREATE TABLE` with it.
Requiring a column definition to end at the next comma or the closing paren is what tells the two
apart, so a column genuinely named `check_flag` or `unique_id` is still read as a column.

### The pre-SQL data commands

`TOTAL`, `JOIN WITH`, `UPDATE ON`, `COPY STRUCTURE`, `DELETE TAG` and `BLANK`. Each names a table, a
field or a variable, so each keeps its operands rather than being recognised and discarded. `DELETE
TAG` had been the worse kind of gap: it parsed as an xbase `DELETE` with a scope of `TAG` and left the
tag name to the catch-all.

### A multi-target `STORE` where a target is subscripted

`STORE 0 TO a[1], b[2]`. The list used to be read as names first and the subscript only when it was
the whole tail, so `STORE 0 TO lnX, laY[3]` booked `laY` as a plain write and read `[3]` on as a
bracket string literal on a statement of its own — a misparse, silently. Each member of the list is
now a target in its own right, and `StoreStatement` carries `targets` rather than a single `target`.

### Memory variables, debugging, screen and menu

`SAVE TO`, `RESTORE FROM`, `PRIVATE ALL EXCEPT`, `ASSERT` and `PLAY MACRO`. `PRIVATE ALL EXCEPT` hides
the caller's variables like the other `PRIVATE ALL` forms, so it now sets the same flag on the scope.

`DEFINE WINDOW`/`MENU`/`PAD`/`POPUP`/`BAR`, `ACTIVATE`/`DEACTIVATE`/`SHOW`/`HIDE`/`MOVE`/`SIZE`/`ZOOM`
of a window, menu, popup or the screen, `ON SELECTION`, and `SET SKIP OF`. None of these reaches a
table or a variable, so each keeps its name and leaves the option tail as source — except the handler
after `ON SELECTION`, which is real code and is parsed as a statement.

### `SET ORDER TO TAG`

Fixed: `TAG` was read as the index file name and the tag name was left to the catch-all, which made
the statement look parsed while the selection was wrong. `USE ... ORDER` already guarded this; the
`SET` form now guards it the same way.

### Repository housekeeping

No change to what the linter reports. `client/` and `server/` are now Bun workspaces of the root
package, so there is one lockfile and one install instead of three and a `postinstall` chain, and CI
no longer has to diff the lockfiles to make `--frozen-lockfile` mean anything. The grammar loses six
rules nothing referenced, the two keywords `Keyword` listed twice, and five copies of the same
select-clause lookahead, now the single rule `SelectClauseKeyword` — `SelectCore` keeps a shorter list
of its own, since `NOCONSOLE`, `PLAIN`, `NOWAIT` and `PREFERENCE` cannot be an alias but are ordinary
column names. `SelectStatement.fromItems` and `DeleteStatement.tables`/`joins`/`fromItems` are gone;
`DELETE ... FROM` now carries the `FromClause` itself, as `SELECT` and `UPDATE` already did.

## 1.2.1

Packaging only — no change to the grammar or the rules. The extension now ships an icon, so it has a
face in the marketplace listing and the installed-extensions list instead of the default placeholder.

## 1.2.0

The release that finishes the symbol table's first pair of rules, and closes every grammar gap the
corpus put a number on.

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
