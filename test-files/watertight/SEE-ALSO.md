# See also — what reading the Watertight source turned up

Two lists, kept apart because they need different owners. The first is for the Watertight
codebase; nothing in it has been changed. The second is for this linter, and each item is already
pinned down by a fixture so it cannot quietly regress.

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
  technically right and probably unwanted — see item 12 below.
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

Three more misparse **silently** — no diagnostic, but the tree is wrong, so anything built on the
symbol table sees the wrong thing. They have no fixture because there is nothing to assert yet:

11. `LOCAL ARRAY laRows[1]` (brackets rather than parentheses) declares a variable literally named
    `ARRAY` and leaves `laRows[1]` as a separate expression statement.
12. `laRows(2) = 5` — assigning to an array element with parentheses — splits into a call expression
    followed by an `=expression` statement, so the write is never attributed to `laRows`.
13. `SELECT ... INTO DBF(m.file)` reads the destination as the literal name `DBF` and leaves
    `(m.file)` as a separate statement.

And one rule-design question rather than a gap:

14. `duplicate-case` fires on every repeated `CASE .f.`. Given that repeating it is the local way to
    comment out a branch, it may be worth skipping conditions that are constant `.F.`.
