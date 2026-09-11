# See also — what reading the Watertight source turned up

Three lists, kept apart because they need different owners. The first is for the Watertight
codebase; nothing in it has been changed. The second is for this linter, and each item is pinned
down by a fixture so it cannot quietly regress. The third is the subset that produced no diagnostic
at all -- those are pinned by a structural assertion instead, because a diagnostics fixture cannot
see them.

Line numbers are against `W:\DevStaging` as of 2026-09-10.

---

## 1. Suspected defects in Watertight

### Copy-paste in a conditional

**`moscode\AAPRG\M_MNUM.prg:89-90`** — the second line tests and assigns the *first* line's variable,
so a caller that passes `m.AppSharedRootPath` has it silently replaced by `m.AppRootPath`:

```
m.AppRootPath       =IIF(TYPE("m.AppRootPath")="C",m.AppRootPath,"")
m.AppSharedRootPath =IIF(TYPE("m.AppRootPath")="C",m.AppRootPath,"")
```

`this.AppSharedRootPath` is then compared against that value to decide whether to reopen the MOSDBF
tables, so the shared-root path never takes effect and the comparison can never see a change.

**`wt\AAprg\WTMOBILES_LOCATIONS.prg:15`** — both branches are the same expression. The two lines
above it pair `Qty`/`Qty2` and `Amount`/`Amount2`, so the false branch is almost certainly meant to
be `PrecisionRate2`, which means old and new app versions currently get the same rate precision:

```
m.PrecisionRate=IIF(m.AppVersion<1000,m.ProcessObj.PrecisionRate,m.ProcessObj.PrecisionRate)
```

**`wt\AAprg\prnt_00000033.prg:303`** — third of three RGB channels; the two above are
`IIF(...,255,0)` and this one is `IIF(...,255,255)`, so the blue channel is always full.

**`wt275\PROGS\servmenu.prg:1453`** — `IIF(LEN(LTRIM(TRIM(MSREF)))=7, "(E) SAVE ORDER AND", "(E) SAVE ORDER AND")`;
both branches produce the same prompt, so the length test decides nothing.

### Branches that can never be reached

**`moscode\AAPRG\EMAILLIB.prg:387-395`** — return codes 20281 through 20285 are already handled at
lines 289-297 of the same `DO CASE`. The five TLS messages ("Error verifying certificate.", "Could
not find client certificate.", ...) are dead, and those failures report the MIME messages instead.

**`wt\AAprg\TABLERULESCALLS.prg:107`** — `CASE EMPTY(m.CallObj.tmosnum)` repeats line 85 in the same
`DO CASE`, so the follow-up validation (error number 9) never fires and the caller sees error 6.

### Code below an unconditional RETURN

**`wt275\PROGS\deltick.prg:48-52`** — `routecal` is opened at line 48 and `use in (d_routecal)` sits
below the `return` at 51, so the work area leaks on every flex-date lookup.

**`wt\AAprg\fifocalc.prg:221-222`** — `Destroy` returns before `CLOSE TABLES all`, so when the object
had a proddet file handle open its tables are never closed.

**`wt\AAprg\prnt_00000082.prg:52-53`** — the `ELSE && Current` branch returns `.f.` before setting
`m.SCDT`/`m.SLDT`/`m.SNDT`, so selecting the "Current" period on this report always bails out. Reads
like a debugging stub that was left in.

**`wt\AAprg\creditcards.prg:661-664`** and **`wt\AAprg\creditcards2.prg:603-606`** — a `RETURN .f.`
sits above the whole block that locks `mastinfo` and allocates the next credit-card batch number.
Probably deliberate (the message above it just reports the missing batch), but it means the
auto-allocation path is unreachable in both copies.

### Not defects — house idioms worth knowing

- **Clause order in SQL is not fixed.** `GROUP BY` before `WHERE` occurs 164 times; VFP accepts it
  and treats `WHERE` as a row filter regardless of position. The grammar already allows any order.
- **`HAVING` with no `GROUP BY`** occurs 32 times, deliberately, as a post-filter — including a
  self-join in `wt\AAprg\appmain.prg:1619` that drops the rows where both sides match. The linter
  reports this at information severity, which reads right.
- **`CASE .f.`** is how a branch is switched off without deleting it
  (`moscode\AAPRG\VOIPDIAL_WATERTIGHTCLOUD.prg:42-50`, `wt\AAprg\AR_SYSCH.prg:73,77`,
  `wt\AAprg\prnt_00000043.prg:84,87,102`). The `duplicate-case` rule reports every repeat, which is
  technically right and probably unwanted — see item 19 below.
- **`vNETLOK`/`vNETULOK`** (`moscode\AAPRG\LOCKING.prg:53,68`) return `.t.` above their original
  bodies on purpose; the comment says so. `unreachable-code` fires on both.

---

## 2. Grammar and rule gaps this corpus uncovered

Each is reproduced by a fixture, so the expectation file changes the day it is fixed.

| # | Construct | Effect | Fixture |
| --- | --- | --- | --- |
| 1 | `TEXT TO x NOSHOW ADDITIVE TEXTMERGE` — `ADDITIVE` not adjacent to the variable | **the whole file fails to parse**: the option list stops, the `TEXT` rule fails, and the body is read as code | `diagnostics/gap-text-additive-parse-failure.prg` |
| 2 | `SCAN [scope] WHILE ... FOR ...` (WHILE first) | reported as `unterminated-block` at **error** severity, naming a missing `ENDFOR` that was never opened | `diagnostics/gap-scan-while-before-for.prg` |
| 3 | A member or column named with a keyword: `.To`, `.From`, `.Class`, `.Select` | the reference is cut at the dot and the rest of the line falls through; 46 `.to`, 37 `.from`, 13 `.class`, 11 `.select` in the source | `diagnostics/gap-keyword-members.prg` |
| 4 | `CATCH TO m.ErrObj` | the `m.` prefix is not accepted, so `.ErrObj` becomes its own statement; this is the most common spelling in the source (50 uses) | `diagnostics/gap-catch-to-memvar.prg` |
| 5 | `USE IN (alias)`, `USE ... ALIAS (expr)`, `SET RELATION ... INTO (expr)`, `SET ORDER TO ... IN (expr)`, `GO TOP IN (expr)` | a work area named by an expression is only accepted where a *table* name is expected, not where an *alias* is | `diagnostics/gap-parenthesised-targets.prg` |
| 6 | `SET FILTER TO` with no argument (the documented way to clear it) | the bare `TO` is left over | same |
| 7 | `PARAM` / `PARAMETER` (singular) | not recognised; 38 and 4 uses | `../diagnostics/still-unsupported.prg` |
| 8 | `SCATTER` / `GATHER` in all their forms | not recognised; 178 and 76 uses, and they are how a record becomes an object throughout | same |
| 9 | `FLUSH`, `MD`, `REINDEX`, `ALTER TABLE`, `COUNT`, `CONTINUE`, `NODEFAULT` | not recognised | same |
| 10 | `WAIT WINDOW <message> NOWAIT NOCLEAR` | the flags are only accepted *before* the message, which is the opposite of how every call site writes it | same |

## 3. Silent misparses — all fixed

These are the dangerous kind: an unsupported statement at least *says* it was not read, while a
silent misparse looks read. The statement parses, the tree is wrong, and every rule downstream
quietly sees the wrong thing. **A diagnostics fixture cannot catch one**, which is why each is now
pinned by a structural assertion in `run-scope-tests.js` instead.

Three were found by reading the Watertight source:

11. `LOCAL ARRAY laRows[1]` (brackets rather than parentheses) declared a variable literally named
    `ARRAY` and left `laRows[1]` as a separate expression statement. Both delimiters now go through
    one `ArrayDims` rule, which also removed the copy `DimensionItem` was carrying.
12. `laRows(2) = 5` — assigning to an array element with parentheses — split into a call expression
    followed by a stray literal, so the write was never attributed to `laRows`. `LValue` now takes a
    parenthesised subscript as well as a bracketed one.
13. `SELECT ... INTO DBF(m.file)` read the destination as a `DEFAULT` one literally named `DBF` and
    dropped the expression. The `DBF` branch now takes an expression, and a word boundary on
    `TABLE`/`CURSOR`/`ARRAY`/`DBF` fixes the matching case in the other direction: `INTO DBFname`
    was read as `DBF` plus a table called `name`.

Four more came out of sweeping the grammar for the *shapes* those three had:

14. `PUBLIC ARRAY` and `PRIVATE ARRAY` had item 11's bug — the `ARRAY` keyword read as the first
    variable name, the declaration behind it as a stray call. **`startup-settings.prg` in this corpus
    was misparsing on line 5 the whole time it was passing.** Both now carry `isArray`.
15. `STORE 0 TO laRows[1]` matched the bare-name list first, which took `laRows` and left the
    subscript behind — so the `ArrayIndexed` alternative below it was unreachable. Reordered, and the
    parenthesised form added. (`STORE 0 TO a[1], b[2]` is still not read, but it now reports
    `unsupported-syntax` rather than silently dropping the subscript.)
16. `PrivateStatement` had no action on its outer sequence, so it returned
    `["PRIVATE", null, whitespace, decl]` and the junk only stayed invisible because the linter skips
    anything whose `type` is falsy. This is the same bug this file records against
    `ProcedureStatement` in an earlier pass.
17. `FieldsClause`, `DelimitedOptions` and `AppendDelimitedOption` returned their raw match too, so
    `COPY TO x FIELDS a, b` stored `["FIELDS", whitespace, {…}]` where the caller meant `{…}`.

An eighth turned up while building the `implicit-private` rule, and it was the worst of them:

18. Inside `WITH ... ENDWITH`, the leading dot of `.Caption = "x"` was **eaten**, so a property of
    the WITH target parsed as a bare `Identifier` -- indistinguishable from `Caption = "x"`. Every
    property assignment in every `WITH` block was booked in the symbol table as a variable write,
    which is 7 phantom variables in this corpus alone and would be far more in form code. The dot is
    now kept as a `WithMemberExpression`, whose root identifier is a property name while the
    arguments and subscripts inside it stay real references -- `.Objects(m.n).Caption` names two
    properties and reads one variable. The same rule's `AS`/`OF` clause had the index bug as well,
    so `WITH oX AS Form` recorded `asType` as whitespace.

And one was a whole class rather than a construct: twelve captures across the grammar read the wrong
index of a PEG sequence, so the value landed on the whitespace *between* the tokens instead of the
token itself. `SCAN`/`REPLACE` lost their `FOR` and `WHILE` conditions,
`SET <command> TO <value>` lost its value, `DIMENSION`/`LOCAL ARRAY` lost the column count,
`SET RELATION ... IN` lost the alias, `REPLACE`'s scope became `"A"`, `DO FORM ... NAME` was always
`LINKED`, and `DO FORM ... WITH ... TO` lost its variable. Each group now carries an action
returning its labelled value, which removes the class at that site rather than moving it by one.
`ProcedureStatement.returnExpression` came out of the same audit and was removed instead:
`(Statement __)*` always consumes the trailing `RETURN`, so the capture was unreachable and the
field was structurally always null.

---

## 4. A rule-design question rather than a gap

19. `duplicate-case` fires on every repeated `CASE .f.`. Given that repeating it is the local way to
    comment out a branch, it may be worth skipping conditions that are constant `.F.`.
