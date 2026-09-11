# Visual FoxPro Linter

Diagnostics for Visual FoxPro `.prg`, `.mpr` and `.spr` files in VS Code.

The extension parses your code with a real grammar instead of matching patterns, so it can tell a field from a variable and a subquery from a statement.

## What it checks

| Code | Severity | What it reports |
| ---- | -------- | --------------- |
| `syntax-error` | Error | A block terminator with nothing open for it to close, or code the parser could not read |
| `unterminated-block` | Error | A block opener whose terminator is missing |
| `unsupported-syntax` | Information | Valid FoxPro the grammar has not learned yet (configurable) |
| `implicit-private` | Warning | An assignment to an undeclared name, which FoxPro creates as a PRIVATE |
| `unused-local` | Warning | A `LOCAL` that is never read or written |
| `missing-memvar-prefix` | Warning | A variable referenced without `m.` whose name is also used as a field |
| `unreachable-code` | Warning | A statement after `RETURN` / `EXIT` / `LOOP` in the same block |
| `duplicate-case` | Warning | A `CASE` condition identical to an earlier one in the same `DO CASE` |
| `private-all` | Warning | `PRIVATE ALL`, which hides every variable of the caller |
| `unlinked-tables` | Warning | Tables in `FROM` with nothing relating them: a Cartesian product |
| `select-without-into` | Warning | A query with no `INTO` or `TO`, which browses its whole result |
| `having-without-group-by` | Information | `HAVING` with no `GROUP BY`, so it is only a post-filter |
| `try-without-catch` | Information | A `TRY` with neither `CATCH` nor `FINALLY` |
| `empty-branch` | Information | An `IF`, `ELSE`, `CASE` or `OTHERWISE` branch with no statements |

Every diagnostic carries its code. The severity of each rule is a setting (`foxpro.rules`, below), and any single finding can be silenced where it stands:

```foxpro
* vfp-lint-disable-next-line implicit-private
gnHandle = 0
lcOld = SET("EXACT")   && vfp-lint-disable-line missing-memvar-prefix -- SET() is not a field
* vfp-lint-disable unreachable-code
...
* vfp-lint-enable unreachable-code
```

A directive with no code silences every rule on that line or in that region. Anything after `--` is a reason and is not read.

### Quick fixes

Three rules know how to fix themselves, and the lightbulb offers it: `implicit-private` adds a `LOCAL` at the top of the routine, `unused-local` removes the name from its `LOCAL` line (or the line, when it was the only name), and `missing-memvar-prefix` inserts the `m.`. Every diagnostic also offers *Suppress on this line*, which writes the `vfp-lint-disable-next-line` comment for you.

### In the editor

The Outline and breadcrumbs list every procedure, function, class, method, property and `#DEFINE` in the file. Blocks fold: `IF`, `DO WHILE`, `DO CASE` and each `CASE`, `FOR`, `SCAN`, `TRY`, `WITH`, `TEXT`, `DEFINE CLASS`, routines with or without `ENDPROC`, and a `SELECT` written over several lines. Comment toggling uses `&&`, brackets and quotes auto-close, and Enter indents after a block opener.

### Rules that deliberately stay quiet

A few rules would be unusable if they reported everything they could, so they hold back:

- **`missing-memvar-prefix`** only reports names your own file shows being used as a field — a column in a `CREATE`, a `REPLACE` target, an `INSERT` column list, or a reference qualified by an alias the file opens. Flagging every bare reference would flag nearly every line.
- **`unlinked-tables`** treats each table in `FROM` as a node and each condition mentioning two of them as an edge, then reports when the graph splits. A name it cannot attribute to a table, a macro, or a derived table silences it, since any of those could be the missing link. Two tables compared to the same variable count as related.
- **`unused-local`** covers `LOCAL` only. `PUBLIC` and `PRIVATE` are meant to be read by other routines, and an unused parameter is usually just a signature the caller still passes.
- **`empty-branch`** is advisory because comments are not in the syntax tree, so a branch holding only a comment looks empty.

## Settings

| Setting | Default | What it does |
| ------- | ------- | ------------ |
| `foxpro.rules` | see the table above | The severity of each rule: `error`, `warning`, `information`, `hint` or `off` |
| `foxpro.maxNumberOfProblems` | `100` | Caps the diagnostics reported per file |
| `foxpro.unsupportedSyntaxSeverity` | `information` | Deprecated: the same as `foxpro.rules` for `unsupported-syntax` alone |

A rule not named in `foxpro.rules` keeps its default, so a settings file only has to say what it changes:

```json
"foxpro.rules": {
  "implicit-private": "error",
  "empty-branch": "off"
}
```

The grammar does not cover all of FoxPro, so valid code can reach the catch-all rule; `unsupported-syntax` is advisory by default for that reason. `syntax-error` and `unterminated-block` cannot be changed, because both are genuinely wrong.

## What it does not read yet

Still unread, and reported as `unsupported-syntax`: `SET` commands whose argument is a list or carries a clause of its own (`SET SKIP TO ... INTO`, `SET RELATION OFF INTO`, `SET PROCEDURE TO a, b`, `SET CLASSLIB TO ... IN ... ALIAS`), `BROWSE` options past the first, `SAVE WINDOW`/`RESTORE WINDOW`, `INSERT BEFORE`, `FIND`, `COPY INDEXES`, `CREATE VIEW`, `DECLARE` of an array, `WAIT ... TO`, and `DEBUGOUT`. Most of those leave the statement itself parsed and lose only the tail.

Macro substitution is parsed where it appears, and `&lcCmd` counts as a read of `lcCmd`, but a macro's contents are only known at run time, so checks that depend on reading a condition skip it.

If you hit something valid that reports as unsupported, please open an issue with the statement in it.

## Requirements

VS Code 1.101 or later.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for the build, the test suites, and how the grammar, the typed AST and the symbol table fit together.
