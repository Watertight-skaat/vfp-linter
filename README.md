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
| `too-many-arguments` | Warning | A call passing more arguments than the routine declares, which FoxPro refuses at run time |
| `duplicate-routine` | Warning | The same routine name defined in two files, which FoxPro resolves by search order |
| `missing-file` | Warning | An `#INCLUDE`, `SET PROCEDURE`, `SET CLASSLIB`, `DO` or `DO FORM` naming a file that is not there |
| `unreachable-code` | Warning | A statement after `RETURN` / `EXIT` / `LOOP` in the same block |
| `duplicate-case` | Warning | A `CASE` condition identical to an earlier one in the same `DO CASE` |
| `private-all` | Warning | `PRIVATE ALL`, which hides every variable of the caller |
| `unclosed-transaction` | Warning | A `BEGIN TRANSACTION` the routine leaves without an `END TRANSACTION` or `ROLLBACK` |
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

### Across the workspace

The extension indexes every `.prg`, `.mpr`, `.spr` and `.h` in your workspace folders, so it can answer questions about code in a file you do not have open:

- **Go to Definition** (F12) on a `DO`, a call, an `#INCLUDE`, a `SET PROCEDURE TO` or a `SET CLASSLIB TO` jumps to it. A routine defined in the same file wins, then one in a library the file loads with `SET PROCEDURE`, then the rest of the tree — the order FoxPro itself resolves them in.
- **Hover** shows the signature, the parameter names, and the comment block written above the routine, which is what VFP code carries instead of documentation. A `#DEFINE` shows its value.
- **Go to Symbol in Workspace** (Ctrl+T) lists every routine, class, method and constant in the tree.

Names assembled at run time are left alone rather than guessed at. `DO &lcProc`, `DO (lcName)` and a target built by concatenation all report that they are named at run time, because nothing in the source says what they refer to.

The first crawl reads only the headers of each file, which takes well under a second on a tree of several thousand files, and the editor is usable while it runs. Files are re-read as they change, including changes made outside the editor such as a branch switch.

### In the editor

The Outline and breadcrumbs list every procedure, function, class, method, property and `#DEFINE` in the file. Blocks fold: `IF`, `DO WHILE`, `DO CASE` and each `CASE`, `FOR`, `SCAN`, `TRY`, `WITH`, `TEXT`, `DEFINE CLASS`, routines with or without `ENDPROC`, and a `SELECT` written over several lines. Comment toggling uses `&&`, brackets and quotes auto-close, and Enter indents after a block opener.

### Rules that deliberately stay quiet

A few rules would be unusable if they reported everything they could, so they hold back:

- **`missing-memvar-prefix`** only reports names your own file shows being used as a field — a column in a `CREATE`, a `REPLACE` target, an `INSERT` column list, or a reference qualified by an alias the file opens. Flagging every bare reference would flag nearly every line.
- **`unlinked-tables`** treats each table in `FROM` as a node and each condition mentioning two of them as an edge, then reports when the graph splits. A name it cannot attribute to a table, a macro, or a derived table silences it, since any of those could be the missing link. Two tables compared to the same variable count as related.
- **`unused-local`** covers `LOCAL` only. `PUBLIC` and `PRIVATE` are meant to be read by other routines, and an unused parameter is usually just a signature the caller still passes.
- **`unclosed-transaction`** credits a close only to the paths that run it: a `ROLLBACK` in the branch that returns is clean, a commit in the *other* arm of the `IF` is not. A close on any path also ends what it claims about that frame, so the `IF TXNLEVEL() > 0` guard silences it rather than being argued with. It reports a frame that a called routine closes, because it reads one routine at a time.
- **`empty-branch`** is advisory because comments are not in the syntax tree, so a branch holding only a comment looks empty.
- **`too-many-arguments`** reports only over-supply, because passing fewer than a routine declares is legal and common — the rest arrive as `.F.`. Where a name is defined more than once the most permissive definition is measured against, and a definition in the calling file wins outright, since that is the one FoxPro finds first. A name the file also declares as a variable is skipped: a subscript is written like a call, and an array must not be measured against a routine that shares its name.
- **`missing-file`** leaves an absolute path alone whether or not it resolves, because it may name a share this machine cannot see — `#INCLUDE S:\Libs\Shared.h` is not evidence of anything. A bare `DO Foo` is a routine rather than a file, so it is not reported here either.
- **The three rules above need the workspace index**, so they say nothing when `foxpro.workspace.enabled` is off. `too-many-arguments` is the exception: a call and the routine it names are usually in the same file, and that half works either way.

## Settings

| Setting | Default | What it does |
| ------- | ------- | ------------ |
| `foxpro.rules` | see the table above | The severity of each rule: `error`, `warning`, `information`, `hint` or `off` |
| `foxpro.maxNumberOfProblems` | `100` | Caps the diagnostics reported per file |
| `foxpro.unsupportedSyntaxSeverity` | `information` | Deprecated: the same as `foxpro.rules` for `unsupported-syntax` alone |
| `foxpro.workspace.enabled` | `true` | Index the workspace, which is what Go to Definition, Hover and Go to Symbol read |
| `foxpro.workspace.exclude` | `node_modules`, `.git` | Globs the index skips |
| `foxpro.workspace.searchPath` | `[]` | Extra directories a file name is looked for in, the equivalent of `SET PATH` |

A rule not named in `foxpro.rules` keeps its default, so a settings file only has to say what it changes:

```json
"foxpro.rules": {
  "implicit-private": "error",
  "empty-branch": "off"
}
```

Macro substitution is parsed where it appears, and `&lcCmd` counts as a read of `lcCmd`, but a macro's contents are only known at run time, so checks that depend on reading a condition skip it.

If you hit something valid that reports as unsupported, please open an issue with the statement in it.

## Requirements

VS Code 1.101 or later.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for the build, the test suites, and how the grammar, the typed AST and the symbol table fit together.
