# Contributing

## Developing the LSP

- Install [Bun](https://bun.sh), then run `bun install` in this folder. `client/` and `server/` are workspaces of the root package, so one install covers all three.
- Open VS Code on this folder.
- run `bun run dev` to start developing
- Switch to the Run and Debug View in the Sidebar (Ctrl+Shift+D).
- Select `Launch Client` from the drop down (if it is not already).
- Press ▷ to run the launch config (F5).
- In the [Extension Development Host](https://code.visualstudio.com/api/get-started/your-first-extension#:~:text=Then%2C%20inside%20the%20editor%2C%20press%20F5.%20This%20will%20compile%20and%20run%20the%20extension%20in%20a%20new%20Extension%20Development%20Host%20window.) instance of VSCode, open a `.prg` file
- The grammar file can be found at `server/src/foxpro.pegjs`

### Commands

| Command             | What it does                                                           |
| ------------------- | ---------------------------------------------------------------------- |
| `bun install`       | Installs the workspace: root, client and server                        |
| `bun run compile`   | Regenerates the parser, type-checks, and bundles client + server       |
| `bun run dev`       | Watches the grammar, the bundles and both type-check projects          |
| `bun run typecheck` | Type-checks only (esbuild does not type-check)                         |
| `bun run test`        | Runs the five suites below                                           |
| `bun run test:update` | Re-records the expected diagnostics for every fixture                |
| `bun run e2e`       | Launches VS Code and runs the end-to-end suite in `client/src/test`    |

### Tests

| Suite                     | What it asserts                                                                        |
| ------------------------- | -------------------------------------------------------------------------------------- |
| `run-all-tests.js`        | Each fixture's diagnostics match its recorded `.expected` file, exactly                 |
| `run-ast-tests.js`        | `ast.ts` declares exactly the node types and properties the grammar emits               |
| `run-scope-tests.js`      | The contents of the symbol table built from `test-files/scope.prg`                       |
| `run-severity-tests.js`   | Each rule follows `foxpro.rules`; locked rules and syntax errors ignore it; suppression comments; `package.json` and the README name every rule |
| `run-keyword-tests.js`    | Every keyword literal in the grammar still allows an identifier that starts with it       |
| `run-fix-tests.js`        | Each quick fix produces the expected text and makes its own finding go away; the Outline and folding ranges |

Fixtures live in three places. `test-files/*.prg` is the coverage corpus: the grammar is expected to
read all of it. A `.expected` file against one of those records either a grammar gap or a rule
finding — `select.prg` and `macro-sub.prg` carry a run of `select-without-into` lines because they
are SQL syntax fixtures whose queries were written without a destination. Either way the point is
that it is stated out loud rather than passing silently.
`test-files/diagnostics/*.prg` is the opposite — fixtures written to make a rule fire, each paired
with the diagnostics it must produce. One of them, `still-unsupported.prg`, is a deliberate ledger
of constructs the grammar cannot read yet: when one is implemented, `test:update` drops its line and
the diff shows coverage improving.
`test-files/watertight/` is a second corpus, modelled on the code patterns in the Watertight ERP
application rather than on the grammar: same rule as the top level -- it must parse cleanly, so a
`.expected` beside one of those files is a regression -- with its own `diagnostics/` subdirectory
for the rule findings and grammar gaps that need surrounding code to show what they cost. Its
`README.md` says what each file covers and `SEE-ALSO.md` records what reading that source turned up.

A fixture with no `.expected` file must produce nothing. To accept a change, run
`bun run test:update` and review the resulting diff: that diff is the point, because it makes a
changed severity or message visible rather than silently absorbed.

> Use `bun run test`, not `bun test`. `bun test` is Bun's own test runner and ignores the
> `test` script -- it picks up the suites under `client/src/test`, which need a running
> VS Code host, and reports failures that mean nothing outside `bun run e2e`.

**A diagnostics fixture cannot catch a statement that parses into the wrong tree.** It asserts what
the linter *reports*, and a misparse that still produces a valid tree reports nothing -- several have
hidden behind a fully passing corpus. When a grammar change alters what a node carries, assert the
shape in `run-scope-tests.js` as well: counting the symbol table's reads and writes is the only check
that sees it.

### CI

`.github/workflows/ci.yml` runs on every push to `master` and every pull request:
`bun install --frozen-lockfile`, then `bun run compile`, then `bun run test`, on pinned bun and
node 22. `client/` and `server/` are workspaces of the root package, so there is one lockfile and
`--frozen-lockfile` is the whole guard.

`bun run e2e` is not part of CI: it downloads VS Code and needs a display.

### The parser is generated, not tracked

`server/src/parser.js` and `server/src/parser.d.ts` are built from `server/src/foxpro.pegjs` and are
**not in git**. They were tracked until two rounds of grammar auditing made the cost plain: a 107-line
grammar change carried a 3,000-line parser diff around it, burying the review that mattered.

`postinstall` runs `bun run build:parser`, so a fresh clone is ready as soon as `bun install`
finishes and `bun run test` works without a separate build step. If you ever see
`Cannot find module '../server/src/parser.js'`, run `bun run build:parser`.

### Build output

`scripts/build.mjs` bundles `client/src/extension.ts` and `server/src/server.ts` with
esbuild into two self-contained CommonJS files. The packaged VSIX therefore contains no
`node_modules` at all -- only `client/out/extension.js`, `server/out/server.js` and the
manifests. `vscode` is the single external, because the extension host injects it.

Do not add `!` negation rules to `.vscodeignore`; they are applied last and will re-include
the whole of `node_modules`.

The parser suite runs straight from source (Bun imports `server/src/linter.ts` directly),
so `bun run test` needs no build step.

### Publishing

`bun run compile && bun run test`, bump `version` in `package.json`, add the release to
`CHANGELOG.md`, then:

```
bunx @vscode/vsce package          # build and inspect the VSIX first
bunx @vscode/vsce publish          # needs VSCE_PAT, or `vsce login <publisher>`
```

`vscode:prepublish` regenerates the parser, type-checks and bundles in production mode, so
packaging never ships a stale parser.

### No ESLint, for now

The toolchain is on TypeScript 7 (the native compiler), and typescript-eslint does not
support it -- it refuses to load and points at
[typescript-eslint#10940](https://github.com/typescript-eslint/typescript-eslint/issues/10940)
for TS >= 7.1 support. ESLint was removed rather than pinning TypeScript back to 5.x.

To compensate, `noUnusedLocals` and `noUnusedParameters` are enabled in `tsconfig.json`, so
the compiler still catches dead locals and parameters -- the one ESLint rule that was
actually earning its keep here. What is no longer checked is stylistic: `no-explicit-any`,
import naming, semicolons.

To bring ESLint back once typescript-eslint supports TS 7, restore `eslint.config.mjs` from
git history and re-add `eslint`, `@eslint/js`, `@stylistic/eslint-plugin`,
`typescript-eslint` and `globals`.

## Structure

```
.
├── client // Language Client
│   ├── src
│   │   ├── test // End to End tests for Language Client / Server
│   │   └── extension.ts // Language Client entry point
├── language-configuration.json // Comments, brackets and indentation for the editor
├── package.json // The extension manifest, including the foxpro.rules settings schema
└── server // Language Server
    └── src
        ├── ast.ts // Typed AST: a discriminated union over every node the grammar emits
        ├── foxpro.pegjs // The grammar
        ├── linter.ts // lint(text): parse, run the rules, resolve severities, apply suppressions
        ├── outline.ts // Document symbols and folding ranges, read off the tree
        ├── rule.ts // The Rule and RuleContext types, and the helpers every rule shares
        ├── rules/ // One module per family of rules, registered in rules/index.ts
        ├── scope.ts // Per-routine symbol table the scope-dependent rules read
        └── server.ts // Language Server entry point
```

Nothing under `server/src` except `server.ts` imports the language server, so `bun run test` drives
`lint()`, the symbol table and the outline straight from source.

### When diagnostics run

`onDidChangeContent` is debounced: a document has to stop changing for 300 ms before it is re-linted,
and each document has its own timer so typing in one file does not hold back another. Closing a
document cancels its pending timer, and the timer re-reads the document rather than capturing it, so a
file that changed or closed while the timer ran is never linted from a stale snapshot.

This is about keystroke latency, not throughput. Parsing is not a bottleneck — a 27,000-line file
takes about 250 ms to parse and under 9 ms to lint — but without the debounce every keystroke queued a
parse of the whole file. Measured on a burst of eleven edits 40 ms apart: eleven parses and eleven
`publishDiagnostics` before, one after.

A settings change re-lints through the same path, so it coalesces with whatever the typist has pending
rather than racing it.

### Rules

A rule is an object with a `code`, a default `severity` and a `check`, made with `onNode()` for a rule
that wants every node of some types or `onFile()` for one that runs once with the whole program. It
reports through `ctx.report(location, message, fix?)`; the context also carries the symbol table, the
source lines and the file's line ending, which is what a fix needs to build an edit. `linter.ts` makes
one traversal for every node rule, runs the file rules, resolves each rule's severity from
`foxpro.rules` (a rule marked `locked` ignores the settings), drops what a suppression comment covers,
and sorts what is left.

To add a rule: write it in the module its family lives in, add it to `rules/index.ts`, add its code and
default to the `foxpro.rules` schema in `package.json`, and add a row to the README table.
`run-severity-tests.js` fails until the last two are done, so the settings UI and the docs cannot
drift from the registry.

A fix is a title and a list of edits. It travels on the diagnostic's `data`, which the client hands back
untouched with a code-action request, so `server.ts` never recomputes anything: it wraps the fix as a
quick fix and adds the generic *Suppress on this line* action beside it. A fix must be safe where it
lands -- the `LOCAL` for `implicit-private` goes at the top of the routine rather than above the first
write, because `LOCAL` resets the variable and the first write is often inside a loop -- and
`run-fix-tests.js` asserts each one by applying it and linting the result.

The user-facing list of rules and what they report is in [README.md](README.md). Three implementation
notes that do not belong there:

**`implicit-private`** reports a symbol whose kind is `implicit` -- nothing declared it -- and that is
written at least once. It fires once per name, at the first write, because the finding is the missing
declaration rather than each use of it; writes marked `sqlContext` do not count, since a bare name in
SQL may be a column. It needed one grammar fix to be usable: inside `WITH ... ENDWITH` the leading dot
of `.Caption = "x"` was being dropped, so every property assignment in every `WITH` block looked like
an assignment to an undeclared variable.

**`missing-memvar-prefix`** also reads the symbol table. It collects the field names the
file itself reveals — `ColumnDefinition` names, `REPLACE` targets, `INSERT` column lists, and
`MemberExpression`s qualified by an alias the file opens — and reports a declared variable referenced
without `m.` whose name is in that set. References inside SQL statements are skipped, because
`scope.ts` marks them `sqlContext` and a bare name there is expected to be a column.

**`select-without-into`** walks only the `body` arrays of `Program`, `BlockStatement` and `DefineClass`,
which is what makes subqueries, `EXISTS`, `IN (SELECT ...)` and `INSERT ... SELECT` fall out without a
suppression list. A `UNION` keeps its `INTO` on the last `SELECT`, so the union members are checked too,
and `isWorkAreaSwitch()` keeps `SELECT 0` and `SELECT myalias` out of it.

### Grammar coverage

A statement the grammar cannot read is a statement no rule can check, so coverage is not cosmetic —
every gap silently subtracts from every rule. Recently added: `TEXT ... ENDTEXT` (the body is held
verbatim and never parsed as code), the `ON ERROR`/`ESCAPE`/`SHUTDOWN`/`READERROR`/`PAGE`/`KEY LABEL`
handlers, `THROW` as a statement, the SQL `CASE WHEN` expression, the `::` scope-resolution operator,
`@ ... SAY`/`GET`/`TO`/`CLEAR`, and the housekeeping commands `CLEAR`, `CLOSE`, `RELEASE`, `PACK`,
`SEEK`, `SUSPEND`, `RESUME`, `KEYBOARD`, `LIST`/`DISPLAY`, `REPORT`/`LABEL FORM` and `SORT`.

Two conventions worth knowing before adding more:

**None of these command words is reserved.** `CLEAR`, `LIST`, `SEEK` and the rest are all legal
variable and function names in FoxPro, so each rule starts with `NotCallOrAssign` and sits after
`AssignmentStatement` in the `Statement` list. `list = 1` is an assignment and `SEEK(lcKey)` is a
call, and there is a test that probes every keyword literal in the grammar as an identifier prefix.

**Long option tails are kept as raw source, not modelled.** `@ ... SAY`, `LIST`, `REPORT FORM` and
`SORT` have large, order-free option lists; those rules capture the remainder of the line into an
`options` string. Recognising the statement is what stops the false positive, and the text is kept so
a rule can look at it later. The operands that carry meaning — coordinates, the GET variable, the
report name, the sort fields — are parsed properly.

One trap that has produced several bugs: in a Peggy action, `(Rule)*` yields the rule's own results,
while `(Rule _)*` yields pairs. Indexing the first as if it were the second silently empties the list —
that is how every `DO CASE` came to drop all of its branches. A body guarded as
`((!terminator Statement) __)*` has the same shape problem, since `flatten` then receives
`[undefined, statement]`; give the group an action that returns the statement instead.

### The typed AST

`parse()` returns `any`, so `ast.ts` declares what it actually hands back: a discriminated union
over all 101 node types, which lets a rule `switch (node.type)` and get a checked set of properties
instead of indexing into an untyped bag.

A hand-written union is only worth having while it is true, so `run-ast-tests.js` re-derives the
node names and property names from `foxpro.pegjs` and fails if `ast.ts` disagrees — a missing node,
an invented one, a renamed property, or a node left out of the `Statement`/`Expr` unions. The
grammar is the source of truth; the union is checked against it on every test run.

Properties are typed from the grammar wherever its shape is fixed. A handful of option bags whose
shape depends on which clause matched are typed `unknown` on purpose: that forces a rule to narrow
rather than trust a guess, which is the one thing `any` would not do.

Watch for a property named `type` in a `node()` call. `node()` is
`Object.assign({ type, location }, props)`, so a `type` in `props` overwrites the node's own type and
erases it — that bug hid four node kinds at once, and it is why the AS clause of a declaration is
called `asType` and the TYPE clause of `COPY TO` is called `exportType`.

### The symbol table

`scope.ts` turns the AST into one scope per routine: `(main)` for file-level code, one per
`PROCEDURE`/`FUNCTION`, one per `DEFINE CLASS` and one per method. Each scope records what it
declares (`LOCAL`, `PUBLIC`, `PRIVATE`, `LPARAMETERS`/`PARAMETERS`, `DIMENSION`, class
properties) with the declared type and site, every read and write of every name, and the
work-area changes in source order so `aliasInEffectAt()` can say which alias is current.

`missing-memvar-prefix` reads it today. Still to come from the same structure: unused `LOCAL`, the
implicit `PRIVATE` created by an undeclared assignment, and work-area handling.
`test-files/run-scope-tests.js` asserts its contents against `test-files/scope.prg`.

Two deliberate subtleties. A name that was never declared still gets an entry, with kind
`implicit` and no declaration site -- that is exactly the implicit-`PRIVATE` case, and it keeps
use-before-declare visible. And a bare name inside a SQL statement is marked `sqlContext`,
because it may be a column rather than a variable; rules should read that as ambiguous, enough
to call a local "used" but not enough to claim the variable was really touched.
