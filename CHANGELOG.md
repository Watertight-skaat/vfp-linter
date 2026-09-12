# Changelog

## 1.3.9

### A transaction that is never closed

`BEGIN TRANSACTION` holds every buffered write and every record lock until the frame closes, so an exit
that steps over the close leaves them held -- the writes uncommitted and the locks on records nobody is
looking at any more. The frame has been one node since 1.3.8; this is the rule that was waiting behind it.

**What it reports.** Two shapes. A `BEGIN` whose routine ends without an `END TRANSACTION` or a
`ROLLBACK`, reported on the `BEGIN`. And a `RETURN` that leaves while a frame is open, reported on the
`RETURN` with the line the frame was opened on -- the early exit from the middle of a frame, which is the
shape that is actually written by accident.

**What it credits.** A close counts only for the paths that run it. Each branch of a statement is followed
from the state the statement is reached in, so the `ROLLBACK` inside the branch that returns is clean, one
in a `CATCH` or a `FINALLY` is clean, and a commit in the *other* arm of the `IF` is not: that arm cannot
run on the path that returns. A close on any path then ends what the rule claims about that frame, which is
what makes `IF TXNLEVEL() > 0` / `ROLLBACK` -- the careful spelling, because a `ROLLBACK` with nothing open
is itself an error -- silence it rather than something to be argued with. Blocks are found by shape rather
than by node type: any `BlockStatement` a statement holds is a branch of it, so a block-bearing statement
the grammar learns later arrives already covered.

The one shape it cannot tell from a forgotten close is a frame a *called* routine closes; the walk stops at
the routine boundary, so that is reported. Splitting a frame across routines is worth a second look anyway,
and `foxpro.rules` can turn the rule off for a file that means it.

**The count was not taken.** The practice on a new rule is to count its hits over the Watertight source
first, and `W:\DevStaging` is not reachable from here, so this shipped on the shape rather than on a
number. What can be said: the corpus models a codebase with no transactions in it at all, and the rule says
nothing on any of the other 112 fixtures, so it costs that source nothing either way. Worth re-counting when
the tree is to hand -- the frame is usually a few lines long, and if the shape does not occur there the rule
earns its keep only for the public extension.

Nine fixture cases in `test-files/diagnostics/transaction-frame.prg`, five of them clean counterparts: the
guarded unwind, the `FINALLY`, the `CATCH`, the `ROLLBACK` that precedes its own `RETURN`, and a closed
frame at file level. Each of the four findings, and each of the guards that keeps the five quiet, was shown
to fail against a deliberately broken version of the walk -- following a branch from a clean slate,
crediting a conditional close to every path, not descending into branches at all, and not stopping at the
routine boundary. The last of those also settled a question reading could not: node properties are *not* in
source order. A walk that takes them as they come reaches a block that starts later before one that starts
earlier in nine corpus files, and does the same with an expression in five more -- an `IF` hands over its
`ELSE` arm ahead of its own. So the frame is followed through the block structure rather than over a flat
list of statements put back in order by position, and the verdict cannot turn on which arm the grammar
happens to hand over first.

## 1.3.8

### The rest of the ledger: the container, its transaction, and the console

Nine constructs, none of which misparsed -- each announced itself and cost the one statement it was on.
That is the cheap failure, but it is still a statement a rule cannot see, and these were the last of the
unmeasured list.

**Transactions.** `BEGIN TRANSACTION`, `END TRANSACTION` and `ROLLBACK` are one node carrying the
action, so a rule can follow the sequence without knowing three node types. They were the only ledger
item with a rule waiting behind them: a buffered write that is never committed now has something to hang
off. `ROLLBACK` stands alone as a word and so refuses every shape a variable of that name takes; the
two-word forms are told apart by their second word and need no guard.

**The database container.** `CREATE`/`OPEN DATABASE`, `CREATE`/`DELETE CONNECTION`,
`DELETE DATABASE`/`VIEW` and `FREE`/`REMOVE TABLE` are one node carrying the verb and the object it
reached, rather than seven node types for what is one shape. The three `DELETE` forms were partials:
`DELETE` parsed as the xbase record command and the object clause after it was what fell to the
catch-all. `CREATE VIEW` is deliberately left out -- that is the SQL view, and claiming it here would
turn a view whose `SELECT` cannot be read into a silently wrong tree instead of a reported gap. The name
may be `?`, which names nothing and asks the user to pick; a macro-substituted expression, whose
variable is then a read like any other; or a path carrying a drive and an extension.

**Console input.** `INPUT` and `ACCEPT` put what the user typed in the variable, which is a write the
symbol table never saw -- the shape `MENU TO` had. The prompt beside it is an expression, so the names in
it are reads. The fixture leaves one of the two variables undeclared on purpose: the `implicit-private`
it earns is the proof that the write is seen.

**`READ` and `@ ... EDIT`.** The obsolete screen command and the multi-line `GET` that fills it.
`CYCLE` is kept, because it is the difference between a read that restarts at the first `GET` and one
that falls through; the rest of the tail varies by control and stays raw source. The bare command is
claimed too -- a name on its own is never a statement, so nothing is taken from a variable called
`read`, and the shapes one does take are refused. `READ EVENTS` and `READ MENU TO` are still their
own statements. `EDIT`'s operand is the same reference `GET`'s is, so the variable it edits reaches the
symbol table.

Thirty-six parse checks, fourteen of them controls, and three scope checks, all run against a parser
built from the previous grammar to confirm they fail there: the twenty-two guarding new behaviour fail or
throw and the fourteen controls pass. `test-files/database.prg` is a new fixture for the container and
the transaction frame; the console commands and `READ` join `commands.prg`, `@ ... EDIT` joins
`at-say-get.prg`, and a `ConsoleInput` routine joins `scope.prg`. Nine lines leave the ledger, and a
tenth goes with them: bare `?` was recorded as unsupported and has in fact been read all along.

## 1.3.7

### Two gaps that cost more than the statement they were in

Both read part of what they were given and stopped, which is the expensive failure: the statement looks
read, so nothing says the node above it is short.

**`CAST(x AS C(<expr>))` -- a computed width.** `TypeSpec` read the width as a number literal, and the
widths in this source come from the schema at runtime: the query pads each column to whatever the target
declares, so the width is a variable. Unread, it did not cost the cast -- it cost the **whole `SELECT`**,
whose destination, joins and `WHERE` were then invisible to every rule that asks about a query. The width
and scale are expressions now, the way `CREATE TABLE`'s `FieldSize` already read them, so a literal width
returns exactly the node it did before. This was the last whole-query-scale gap in the ledger.

**`DO FORM <a-b>`.** A form is named by a file, and the name was read as an identifier, so it stopped at
the first character an identifier cannot hold: `DO FORM start-up_code_mod` ran the form called `start` and
left `-up_code_mod` to be reported as a statement of its own, and `DO FORM myform.scx` did the same at the
dot. A name carrying a hyphen, a dot or a directory is now read as a file name, with `DO FORM (m.cForm)`
-- the documented way to name one at runtime -- read as the expression it is. A plain name is still a
plain name and a quoted one still a string.

Twelve parse checks, five of them controls, all run against a parser built from the previous grammar to
confirm they fail there: the seven guarding new behaviour fail or throw, the five controls pass. The
`gap-cast-computed-width` fixture keeps its expectation file deleted, which is how a fixed gap is
asserted, and the `DO FORM` line leaves the ledger.

## 1.3.6

### ALTER TABLE's tail, read as clauses

The last of the older grammar items. `CREATE TABLE` learned its table-level constraints a release ago;
`ALTER TABLE` kept its whole tail as source, on the grounds that the table was the part a rule would ask
about. That cost it twice.

**A clause on a continuation line was left behind.** The raw tail stopped at the physical line, so
`ALTER TABLE items ADD COLUMN billcode C(6) ;` followed by a second `ADD COLUMN` reported the second one
as a statement of its own that the linter cannot parse -- a gap announced in the middle of a statement
that had in fact been read. The raw form now crosses a semicolon, which is what a semicolon means, so
every command that keeps an option tail verbatim -- `@ ... SAY`, `MODIFY`, `DEFINE`, `PUSH`/`POP` --
gets the rest of its own line back.

**The columns it adds were invisible.** `missing-memvar-prefix` asks what the file shows being used as a
field, and a column in a `CREATE` counted while one added by `ALTER` did not, so a `LOCAL` colliding with
it went unreported. The tail is now a list of clauses: `ADD`/`ALTER`/`DROP`/`RENAME COLUMN`, the
constraint forms added and dropped, `SET`/`DROP CHECK` and `NOVALIDATE`, with the column definition
`CREATE TABLE` already reads behind the first two and the expressions in `DEFAULT` and `CHECK` kept, in
SQL context, so a bare name is taken for a field and `m.cValue` still records the variable it reads. A
tail holding a clause the list cannot read falls back to the raw form, so an unrecognised one still
costs nothing.

**`NOT NULL` was read as `NULL`.** Found by the fixture for the above. The two spellings were one string
literal each and the action told them apart by the *shape* of what the alternative returned -- an array
for one, a string for the other -- which both literals returned as a string, so every `NOT NULL` column
in a `CREATE TABLE` or an `ALTER` reported the opposite of what it says. They are now one rule that names
which matched.

Sixteen parse checks assert each on the field that was lost, with two controls beside them, and all
eighteen were run against a parser built from the previous grammar to confirm they fail there: the
sixteen guarding new behaviour fail or throw, the two controls pass.

## 1.3.5

### Three rules that read less than their own comment claimed

None of these is a missing command. Each is a rule that already exists and reads part of its statement,
which is the expensive kind of gap: the statement looks read, so the tail it drops is reported as a
separate unsupported one and nothing says the node above it is short.

**`TOTAL TO <file> ON <key>`**, the order the documentation leads with. Only the reverse was read, so
the canonical spelling fell to the catch-all whole. The two halves are now one alternative either way
round, with the same option tail behind them.

**Three `SET`s whose argument ran past the setting reader.** `SET TOPIC ID TO 5` and
`SET NOTIFY CURSOR OFF` are two-word settings: the second word was taken for the setting itself and
everything after it left behind, so `TO 5` and `CURSOR OFF` were reported as statements of their own.
`SET WINDOW OF MEMO notes TO myform` is the only `SET` whose operand sits between its keywords, so it
gets its own rule beside `SET SKIP OF` and `SET MARK OF` -- a `SetWindowOfMemo` node naming the field
and the window, with `window` null for the bare `TO` that restores the default one.

**`REPLACE ... RECORD 5`.** The scope is documented *after* the field list and only the leading
`ALL | REST` was read. `REPLACE` now takes the same order-free option set as `SCAN`, so `RECORD n`,
`NEXT n`, `FOR`, `WHILE`, `IN` and `NOOPTIMIZE` read in any order in either position. The `NEXT`
spelling was the one that cost more than a statement: unread it fell to the dangling-terminator rule
and closed the enclosing `FOR`, which lost the loop every statement after it. Adding a word boundary to
the leading `ALL` fixed a second defect found by its control test -- `REPLACE allowance WITH 0` was
reading `ALL` as the scope and `owance` as the field.

Fourteen parse checks assert each on the field that was lost, with a control beside it, and all
fourteen were run against the previous grammar to confirm they fail there.

## 1.3.4

### The measured half of the ledger

The constructs that reported themselves rather than parsing. None of them cost a file -- each cost
one statement, which is the acceptable failure mode -- so the case for reading them was volume, and
that is what the sweep over the Watertight source measured. Every measured one is read now, and what
is left in `still-unsupported.prg` is unmeasured.

**`@ <row>, <col>` with no verb** was the most common `@` in that source at 147 uses: the print-head
move the report code emits between lines. The bare form has to end the line, which is what keeps a verb
the grammar has not learned -- `@ 3, 2 EDIT m.cUsed SIZE 17, 75 NOEDIT` -- reporting as one gap instead
of splitting into a statement that looks read and an orphaned tail. `verb` is `null` for it.

**The console commands.** `EJECT`, `EJECT PAGE`, `RETRY`, `SHOW GETS` and `SHOW GET m.answer`.
`RETRY` re-runs the statement that raised the error and so does not fall through, but where it resumes
is the caller's business, so it is deliberately not treated as a block terminator the way `CANCEL` is.
`SHOW GET` keeps the variable it names, which is a read the symbol table was losing.

**`AS <type> [OF <library>]` on `PRIVATE` and `PUBLIC`.** Neither documents the clause; the code
writes it anyway and VFP accepts it, so the name was read and the type fell off as a statement of its
own. The type now reaches the symbol table as the declared type, the same as `LOCAL`'s. `OF` reads a
file name on all three, so `LOCAL loSA AS SECURITY_ATTRIBUTES OF oplocks.prg` no longer stops at the
dot and leaves `.prg` behind. One `AsClause` rule replaced three copies of the same inline clause.

**`DELETE RECORD <n> IN <alias>`**, 59 uses. The xbase form read its scope as a bare identifier, so
`ALL` worked and a record number did not; it now uses the same scope rule as `COPY TO`, which reads
`RECORD RECNO("rec2inv")` and `NEXT 5` as well. `RECALL` had the identical defect and the same fix.
A side effect worth having: `DELETE VIEW myview` used to read `VIEW` as the record scope, so the gap
it reported was the name alone; it now reports the whole clause it could not read.

**Four clauses whose operand is an expression rather than a name.** `FLUSH IN (m.inWorkArea) FORCE`
had no `IN` clause at all. `SET ORDER TO IIF(TYPE("m.cOrder") = "U", "servsnum", m.cOrder)` took the
function's name for an index file and left its arguments behind. `MD (ADDBS(m.m_tpath) + "temp")` and
`MODIFY COMMAND (m.cFile)` were refused outright by the guard that keeps `MD(x)` a call to a function
of that name; the parenthesis is now refused only when it is glued to the word, which is what tells the
two apart.

**`IF <cond> THEN`**, which the 2000s layer writes and the rest does not. The `IF` itself parsed, so
the cost was a stray statement -- but one the symbol table booked as a read of a variable named `THEN`.
It is claimed on the condition's own line only, so `THEN = .F.` on the line below is still an
assignment to a variable of that name.

**`DIMEN`**, and every other spelling down to the four letters VFP allows.

Each has its parse asserted in `run-parse-tests.js` on the field that was lost, with a control beside
it, because widening a rule is how the next silent misparse gets in. The constructs themselves moved
out of the ledger and into the fixtures, where an `.expected` file appearing beside one is the
regression.

### Records that had gone stale

`ACTIVATE SCREEN`, `READ EVENTS`, `CANCEL`, `ADD OBJECT ... AS`, `SET RELATION OFF INTO` and a
method's declared return type were all recorded as unread and all parse. So were the pre-SQL data
commands, `STORE 0 TO a[1], b[2]`, the memory-variable and debugging commands, and the screen and menu
commands, each of which had a bullet in the TODO claiming otherwise. Probing the parser one construct
at a time is what turned them up; reading the grammar is what let them go stale.

## 1.3.3

### The thirteen constructs that cost a whole file

Running the linter over all 1747 `.prg` files in the Watertight source found 47 that failed to parse
outright, and thirteen constructs accounted for 43 of them. A file that does not parse is a file where
no rule runs at all, and the error it reports is never on the line at fault: PEG rejects a block
wholesale when anything inside it fails, so the opener falls to the unsupported catch-all and the error
surfaces on an orphaned `ELSE` or `ENDIF` hundreds of lines below. All thirteen are read now.

**A leading-dot member reference inside an expression** was the largest of them on its own -- more
whole-file failures than the rest put together. The dot was read only where a statement *started* with
it, so `IF .ChartsCount > 1` and `CASE .Mode = 1` failed their condition and took the whole `IF` or
`DO CASE` with them. It is now read in `PostfixExpression`, which also gives `WITH .Fields(n)` a target
it can name; `.T.`, `.NULL.` and `.5` still read as literals, which is what the member name is checked
for. `LValue` reads it too, and that fixed a second gap in the same place: `.Width = 400` under an `IF`
inside a `WITH` used to report as unsupported, so form code -- which conditions most of its property
writes -- showed the symbol table a fraction of what a `WITH` block actually writes.

**`PARAMETERS()`**, the function that returns how many arguments the caller passed, lost to the
declaration keyword of the same name. It is the only way the legacy code defaults an optional argument,
so `IF PARAMETERS() < 3` cost the file. The opening parenthesis tells the two apart, the same way it
already did for `SELECT()`.

**A second `CATCH` in one `TRY`** is the shape of every retry loop here -- the first narrowed by `WHEN`
to the error it can recover from, the second taking everything else -- and only one was read. `TRY` now
carries every clause in source order.

**A method's return type without a parameter list** (`HIDDEN FUNCTION Release AS Logical`) left
`AS Logical` behind as a statement of its own. The `PROTECTED`/`HIDDEN` prefix itself was already read.

**`DO CASE <expression>`**, which VFP ignores and the old code writes as documentation of what is being
switched on, is kept rather than discarded, so the read still reaches the symbol table. **A second
`OTHERWISE`** is read as well: VFP runs the first and the rest are dead, so refusing them cost the file
for nothing. They are kept apart from the live branch rather than merged into it.

**`LOOP` and `CLASS` as ordinary names.** Neither is reserved in VFP, and both are column names in the
metadata tables and flag variables in the 1990s code. `LOOP`'s own statement rule now refuses every
shape a variable of that name takes, so `loop = .f.` is an assignment and a bare `LOOP` is still the
loop-control word.

**`DEFINE CLASS X` with no `AS`** defaults the parent to `Custom` in VFP; the clause was required here.
**`COPY TO <file> NEXT <n>`** was unread, and because `NEXT` also terminates a `FOR` the leftover could
not even fall through to the catch-all -- it closed the enclosing `DO WHILE`. The record count reads as
an expression, because the chunking loops write `NEXT (m.nChunk)`.

**A `#IF` fence that does not nest with the block structure around it** was the odd one out: the
preprocessor is a text pass that runs before the compiler, so `IF` outside a fence and its `ENDIF`
inside one is legal, and a block node cannot represent it. A directive that cannot nest now stands
alone, which leaves the code's own blocks nesting correctly and stops the catch-all reporting a
statement it had in fact read.

Each of the thirteen had a fixture recording the gap; all thirteen now parse clean, and the tree each
one produces is asserted in `run-parse-tests.js` -- a clean fixture cannot tell a correct parse from a
misparse, which is the failure mode that matters most here.

### Two records that had gone stale

`SELECT()` the function and a `#IF` nested inside another `#IF` were both already fixed, and both were
recorded as still broken. So was `BROWSE NORMAL`, whose check in `run-keyword-tests.js` asserted the
old two-statement misparse. The corpus expectations are regenerated, which is what turned all three up.

## 1.3.2

### The last four lines of the unsupported ledger

`CREATE TRIGGER ON customer FOR INSERT AS NewCustomer()` and `DELETE TRIGGER ON customer FOR INSERT`
are the referential-integrity side of the database container, and `VALIDATE DATABASE RECOVER` the check
that it still matches what is on disk. The trigger's expression is parsed as code rather than kept as
text, because it is usually a call into a validation routine. `DELETE TRIGGER` had to be claimed ahead
of `DELETE`, whose xbase form read `TRIGGER` as the record scope and left the rest of the line behind.

`SHUTDOWN` is its own node rather than sharing one with `QUIT`, because `ON SHUTDOWN` runs first and
that is a chance for code to run.

The Foxbase menu system -- `MENU BAR mBar, 5`, `MENU TO lnChoice` and `READ MENU TO lnChoice` -- which
predates `DEFINE POPUP` and still turns up in the oldest files. `MENU TO` puts the number of the chosen
bar in the variable, so the symbol table now books that write and the array `MENU BAR` builds from as a
read; before, both names were invisible.

`RELEASE MENU mMain EXTENDED` and `RELEASE POPUP pFileMenu` were the only ones of the four that
misparsed rather than reported: `RELEASE` read as far as the word, took `MENU` for the name of a
variable to release and left the real name behind. Both spellings, singular and plural, now name the
menu and keep `EXTENDED`.

### Two misparses that nothing reported

`USE customer ORDER TAG custid` parsed, and read every word of the `ORDER` clause as a connection
handle with the last one winning, because `OrderSpec` was reachable only through `USE ... ?`. The clause
now sits above the handle alternative, so the tag, its `OF` file and its direction all read, and the
option after it is no longer lost.

`SET HELP TO x.hlp` parsed too, and read the file as member access on a variable called `x` -- a read of
a name that does not exist, booked against the symbol table and reported by nothing. The `SET`s whose
argument is a file now read it as one. A bare name stays an expression, because `SET CLASSLIB TO mylib`
may well be a variable holding the library, and so do `m.` and `&` and a parenthesised argument, none of
which is ever a file.

## 1.3.1

### `RENAME`'s container forms

`RENAME TABLE oldname TO newname` renames an object inside the database container rather than a file
on disk, and until now reported itself as unsupported. It and its three siblings -- `RENAME VIEW`,
`RENAME CONNECTION` and `RENAME CLASS poster OF posters.vcx TO banner`, whose library keeps its
extension -- are one `RenameObjectStatement` carrying `kind`, and either end may be a name expression
in parentheses. The file form is untouched: `RENAME table.dbf TO new.dbf`, and even a file actually
named `TABLE`, still read as a file.

`CopyFileStatement` and `RenameStatement` were emitted through a ternary, which is the one shape the
AST suite's probe cannot see, so neither had ever been declared in `ast.ts`. Both are now emitted by
name and declared, and the suite checks their properties like every other node's.

## 1.3.0

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

### The last three lines of the ledger

`SAVE SCREEN` / `RESTORE SCREEN`, the pair between `SAVE TO` and `SAVE WINDOW`, which were read as
nothing at all; the `TO` clause of `SET MARK OF`, which puts the tick beside a menu item and was
dropped while the rest of the line parsed; and an index file named with its extension --
`INDEX ON custid TAG custid OF cust.cdx` -- where the name was read as an identifier that stopped at
the dot, losing the file and leaving `.cdx ADDITIVE` to be read as a statement of its own.

The last of those sat behind every index file name, not only this one: `USE ... INDEX cust.idx`,
`SET ORDER TO TAG x OF cust.cdx` and `COPY INDEXES` lost their files the same way. All of them now go
through one rule that claims a name with a dot, a drive or a directory in it as a path first.

### `FIELDS LIKE` and `FIELDS EXCEPT`

Both were written into the grammar below the plain field list, which matches `LIKE` as a field name of
its own, so neither alternative could ever be reached: `COPY TO x FIELDS LIKE c*` read as one field
called `LIKE` and left the skeleton behind. Found while probing for what else the ledger should hold.

### The ledger fixture

Refilled, with nine constructs the same probe turned up and nobody had recorded: `CREATE TRIGGER` /
`DELETE TRIGGER`, `VALIDATE DATABASE`, `RENAME TABLE` / `RENAME CLASS` (since read), `SHUTDOWN`, the Foxbase
`MENU BAR` / `MENU TO` / `READ MENU TO`, and `RELEASE MENU` / `RELEASE POPUP`, which reads as far as
the word and then takes `MENU` for the name of a variable to release.

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
