# Watertight-shaped fixtures

A corpus modelled on the code patterns in the Watertight ERP desktop application: a 30-year-old
VFP Advanced codebase with three distinct eras of style layered on top of each other, free DBF
tables opened by name, pessimistic locking, macro-driven metadata, and no transactions anywhere.
Nothing here is copied from that source. Each file reconstructs a *shape* that recurs across it, so
the grammar is exercised against the kind of code the linter actually has to read rather than
against minimal examples.

## Layout

- `*.prg` — the corpus. These must parse cleanly and produce no diagnostics, so any `.expected`
  file appearing beside one of them is a regression.
- `diagnostics/` — fixtures written to make a rule fire on realistic code, and fixtures that record
  a construct the grammar cannot read yet. Their `.expected` files are the assertion; a `gap-*.prg`
  with none is one that has since been fixed, and it must stay clean.

The abstract, one-rule-at-a-time fixtures live in `test-files/diagnostics/`; these are the applied
counterparts. One-line gaps go in that directory's `still-unsupported.prg` ledger, and only the
gaps that need surrounding code to show what they cost get a fixture of their own here.

## What the corpus covers

| File | Pattern |
| --- | --- |
| `singleton-session.prg` | the `TYPE("_screen.MosDA...")#"N"` bootstrap and a `Session` class with `DataSession = 2` |
| `id-allocation.prg` | lock, read, increment, flush, unlock on a counter table; the recompute-the-max fallback |
| `seek-and-scan.prg` | `SET ORDER TO` + `SEEK` on a compound tag, padded fixed-width keys, `SCAN REST` |
| `sql-aggregates.prg` | rollups, `UNION`, a derived table, `INTO CURSOR ... NOFILTER`, shuffled clause order |
| `sql-joins.prg` | parent/child matched on concatenated keys, `LEFT OUTER JOIN`, `EXISTS`, `IN` |
| `sql-destinations.prg` | every `INTO`/`TO` destination, plus a macro field list over a macro table |
| `macro-substitution.prg` | `&var`, `&var->field`, `&alias->&field`, macros as index keys and select lists |
| `legacy-1990s.prg` | `PARAMETERS`, ALL-CAPS `M_`/`D_`/`T_` variables, `=SEEK()`, `APPEND BLANK` + `REPLACE` |
| `record-locking.prg` | `vOpenItem`/`vClosItem`, `RLOCK`, `SET REPROCESS`, the `NETLOK` return-true stubs |
| `error-handling.prg` | the global `ON ERROR` handler's `DO CASE` over error numbers; `TRY`/`CATCH`/`FINALLY` |
| `dotnet-interop.prg` | `CreateDotNetObject`, `CreateJSObject` property bags, clients cached on `_screen` |
| `windows-api.prg` | `DECLARE ... IN win32api` across continuation lines, `@` buffers, `#DEFINE` constants |
| `report-output.prg` | `@ row, col SAY/GET/TO` with the long option tails, output dispatched by job type |
| `text-merge-codegen.prg` | `TEXT ... ENDTEXT` with `TEXTMERGE`/`PRETEXT` building a `SELECT` that is then macro-run |
| `array-work.prg` | `SET COMPATIBLE OFF`, growing `DIMENSION`, `PRIVATE ALL LIKE`, `ALEN`/`ASCAN`/`ASORT` |
| `table-maintenance.prg` | `CREATE TABLE`/`CURSOR`, `APPEND FROM`, `COPY TO`, `INDEX ON ... TAG`, `PACK`, `ZAP` |
| `form-launch.prg` | `DO FORM ... WITH ... TO`, `NAME ... LINKED`, `WITH ... ENDWITH`, `DODEFAULT`, `::` |
| `metadata-dispatch.prg` | menu and table-action rows whose command string is macro-executed; `ON KEY LABEL` |
| `branch-preferences.prg` | per-branch settings read out of a preference table with a typed fallback |
| `string-utilities.prg` | single-letter variables, recursion, character maps built from both quote styles |
| `datalog-audit.prg` | the parallel change-log table and the field-by-field comparison that feeds it |
| `websync-chunk.prg` | a chunk cursor, a macro-assembled query, `SET RELATION`, per-row `TRY` |
| `date-and-period.prg` | billing periods, `{}` and `{^...}` literals, saving and restoring `SET CENTURY` |
| `startup-settings.prg` | the `SET` state, `SET PATH`, class libraries and `PUBLIC` globals established at launch |
| `preprocessor.prg` | `#INCLUDE`, `#DEFINE`, `#IF`/`#ELSE`/`#ENDIF` fencing off unfinished work |
| `invoice-posting.prg` | a multi-table write ordered defensively, each table flushed as it is written |
| `bulk-update.prg` | scoped `REPLACE ALL`, `CALCULATE`, `SUM`, `DELETE ALL FOR`, `RECALL` |
| `edi-segments.prg` | a parser class whose delimiters are properties; `DODEFAULT`, `THROW`, an `Error` method |
| `message-queue.prg` | `FOR EACH ... AS ... IN` over a `Collection`, typed `AS Exception` locals |

## See also

### The 2026-09-13 pass over `W:\Devstaging2`

The tree moved and was relaid out: 1602 indexed files in one repository, laid out by what a file is
rather than by which product it came from. `bun run lint:dir W:\Devstaging2` over it returned 31882
findings, and sorting those produced the second wave of `gap-*.prg` fixtures here plus two workspace
cases under `test-files/workspace/`.

The gaps that wave found, worst first: a string literal that crosses a line break and silently eats
everything to the next quote; keywords abbreviated to four characters (`ENDI`, `DELE`, `ACTI`);
`#IF .F.` bodies parsed as code; `REPLACE` with a computed target; a keyword as a routine name or a
`CASE` subject; `DELETE ... IN <alias> FOR`; a bare `USE` with a trailing `&&` comment;
`AS <dotted.Type>` in a declaration list; the SQL `?` parameter marker; `&alias..field`; and a table
named by an expression.

Three of the fixtures record a *rule* being wrong rather than the grammar --
`system-memory-variables.prg`, and the workspace cases `builtin-shadowing/` and
`resolve-across-tree/`. Their expectations record the false positive, so fixing the rule empties the
file rather than editing it. `TODO.md` carries the counts and the reasoning.

`SEE-ALSO.md` lists what the Watertight source turned up: suspected defects in that codebase, and
the grammar gaps this corpus uncovered. Its first half came from reading; its second half came from
running the linter over all 1747 `.prg` files in that tree and sorting the 47 whole-file parse
failures and 5525 unsupported statements that came back. The `gap-*.prg` fixtures in
`diagnostics/` are that second pass: one per construct that cost a file its parse. Thirteen of the
fourteen are fixed and parse clean now, so they guard the fix rather than record the gap; the tree
each one produces is asserted in `run-parse-tests.ts`, because a clean fixture cannot tell a correct
parse from a misparse. `gap-cast-computed-width.prg` is the one still open.
