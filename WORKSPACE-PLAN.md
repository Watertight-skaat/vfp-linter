# Workspace features: implementation plan

Plan for [WORKSPACE-FEATURES.md](WORKSPACE-FEATURES.md). Each phase lists its tests first, then the code, then what "done" means. Estimates assume one person and the repo's tests-first discipline.

## What the code actually does today (verified, corrections to the brief)

Probed with the real parser, not the grammar file:

- **`DoStatement.target` is a tuple.** `target:(!("FORM"i ...) PathOrExpression)` in the grammar captures the predicate too, so `DO foo` yields `target: [null, {type:'Path', path:'foo'}]` while `ast.ts` declares `Expr | Path | QualifiedTable`. Nothing reads it yet, which is why it went unnoticed. Every DO-based feature below needs this fixed first.
- **`DO x IN file.prg` misparses.** `IN` accepts only an identifier, so `.prg` becomes an `UnknownStatement`. The corpus uses `DO WaitForRlock IN Procfile`, which happens to parse, but the `.prg` form is documented VFP.
- **Target shapes the index will see** (after the tuple fix): `DO foo` and `DO S:\apps\thing.prg` are `Path`; `DO (lcName)` is an `Identifier` (the parenthesised runtime form, so Identifier means dynamic); `DO &lcName` is a `Path` whose text starts with `&`. `DO FORM myform` is the bare string `'myform'`, `DO FORM forms\custedit` a `Path`, `DO FORM (lcForm)` an `Identifier`. `x()` is `CallExpression` with an `Identifier` callee; `this.bar()` has a `MemberExpression` callee; `&lcFn()` a `MacroSubstitute` callee. `#INCLUDE "x.h"` is a `StringLiteral`, unquoted a `Path`. `SET PROCEDURE TO lib1, lib2 ADDITIVE` is `SetCommand{command:'PROCEDURE', arguments:[Identifier, Identifier]}`; a quoted path is a `StringLiteral`, a bare `S:\libs\y.prg` a `Path`. `CREATEOBJECT("MyClass")` is an ordinary call with a `StringLiteral` first argument.
- **`LPARAMETERS` is usually already on the header.** The grammar absorbs an `LPARAMETERS` line into `ProcedureStatement.parameters` even across blank and comment lines. A file-level `LPARAMETERS` (a `.prg` called with `DO file WITH ...`) is a `ParametersDeclaration` in `main`. The fallback in `outline.ts` stays as the one source of truth and moves somewhere both the outline and the index can import.
- **Diagnostics are only published for open documents** and cleared on close. Cross-file findings will follow that model: a finding about B appears when B is open. That makes "re-lint dependents" cheap (only open dependents matter) and is a decision worth stating up front.
- **`run-all-tests.ts` walks every `.prg` under `test-files/`**, so the new `test-files/workspace/` tree has to be excluded from it explicitly.
- The corpus already contains what the rules need to be quiet about: `#INCLUDE S:\Libs\Watertight.WebClients.h` (an absolute path on a drive CI does not have), `SET CLASSLIB TO controls ADDITIVE`, `CREATEOBJECT("moseslib")`, `DO ... IN Procfile`.

## Architecture

```
server/src/
  routine.ts     routineParameters(node) -- moved out of outline.ts, shared by outline, index and rules
  index.ts       pure: header scan, AST extraction, the WorkspaceIndex data structure and its queries. No fs, no LSP.
  workspace.ts   fs: crawl roots, exclude globs, read files, feed the index, tier-2 background queue, disk cache. No LSP.
  dynamic.ts     isDynamic(node): the one macro/runtime-target test every cross-file rule consults
  navigation.ts  pure: nodeAt(ast, pos), definitionAt, hoverAt, referencesAt, completionsAt, signatureAt -- all take (text|ast, position, index) and return plain data
  builtins.ts    the VFP function/command name table (phase 4)
  linter.ts      lint(text, options) gains options.workspace = { index, file }
  rule.ts        RuleContext gains ctx.workspace; LintDiagnostic gains relatedInformation
  server.ts      wires the above to the connection: capabilities, watcher events, progress, dependents re-lint
```

The split matters for the tests: everything except `server.ts` is driven directly by `test/`, and `workspace.ts` is driven by pointing it at a fixture directory.

### The index

```ts
interface RoutineRecord { name: string; key: string /* upper */; isFunction: boolean; params: string[]; loc: Loc; file: string; owner: string | null /* class */; doc: string | null }
interface ClassRecord   { name: string; key: string; base: string | null; methods: RoutineRecord[]; properties: string[]; loc: Loc; file: string }
interface ConstantRecord{ name: string; key: string; value: string | null; loc: Loc; file: string }
type RefKind = 'do' | 'call' | 'form' | 'include' | 'procedure' | 'classlib' | 'object';
interface Reference     { kind: RefKind; name: string; key: string; loc: Loc; file: string; dynamic: boolean; argc: number | null }
interface FileRecord    { file: string; mtime: number; size: number; tier: 1 | 2; routines: RoutineRecord[]; classes: ClassRecord[]; constants: ConstantRecord[]; mainParams: string[] | null; refs: Reference[] }

class WorkspaceIndex {
  roots: string[]; searchPath: string[];
  files: Map<string, FileRecord>;                 // keyed by normalised lower-case absolute path
  known: Set<string>;                             // every file path under the roots, any extension, for resolveFile
  routines: Map<string, RoutineRecord[]>;         // key -> every definition, top-level only (methods live under classes)
  classes: Map<string, ClassRecord[]>; constants: Map<string, ConstantRecord[]>;
  dependents: Map<string, Set<string>>;           // definition key -> files that reference it (the reverse map)
  upsert(record: FileRecord): { changed: Set<string> }  // returns definition keys whose signature appeared, vanished or changed
  remove(file): { changed: Set<string> }
  resolveRoutine(key, fromFile): RoutineRecord[]  // same file, then files named by fromFile's SET PROCEDURE, then everything else
  resolveFile(name, kind: RefKind, fromFile): string | null  // VFP extension defaulting, fromFile dir -> roots -> searchPath, case-insensitive
  dependentsOf(keys: Iterable<string>): Set<string>
}
scanHeader(file, text, stat): FileRecord          // tier 1
extract(file, ast, lines, stat): FileRecord       // tier 2, also used for the open document's live tree
```

**Tier 1 covers more than the brief asks for.** The regex pass reads every line-anchored construct, not only definitions: `DO x [IN y]`, `DO FORM x`, `#INCLUDE`, `SET PROCEDURE TO`, `SET CLASSLIB TO`, plus `PROCEDURE`/`FUNCTION` (with a `PROTECTED`/`HIDDEN` prefix and either parameter style), the `LPARAMETERS`/`PARAMETERS` line that follows, `DEFINE CLASS x AS y` / `ENDDEFINE` (to assign methods to their class), `#DEFINE`, and the comment block above a routine (its docstring, capped at 20 lines). It skips `TEXT ... ENDTEXT` bodies and `*`/`NOTE` comment lines. Only `x()` calls and `CREATEOBJECT`/`NEWOBJECT` literals need the parse. Consequence: the phase 2 rules run entirely off tier 1 plus the current file's own AST; tier 2 is needed only by find-references and `unused-routine`.

**Tier 1 is held to tier 2.** A parity test runs both extractors over every fixture in `test-files/` and asserts identical routine/class/constant/reference sets. Any allowed divergence is listed in the test by file name with a reason. This is the regression guard for the regex.

**Resolution order** follows VFP: the calling file, then the files its `SET PROCEDURE TO` names (transitively, one level is enough in practice), then the rest of the index. `resolveRoutine` returns them in that order so callers can stop at the first tier that hits.

### Where the three settings live

`foxpro.workspace.enabled` (default `true`), `foxpro.workspace.exclude` (globs, default `["**/node_modules/**", "**/.git/**"]`), `foxpro.workspace.searchPath` (string[], VFP `SET PATH` equivalent, relative entries resolved against each root). Read once at `onInitialized` with `connection.workspace.getConfiguration({ section: 'foxpro.workspace' })`; a change to `enabled` or `exclude` re-crawls, a change to `searchPath` only re-lints open documents.

### Stale cross-file diagnostics

`upsert` diffs the old and new record's definitions and returns the keys whose arity, existence or file changed. `server.ts` collects `dependentsOf(changed)`, intersects with open documents, and calls the existing `scheduleValidation` for each, so the 300 ms debounce and the per-document cancel already in place cover the burst. A cap is unnecessary because only open documents are re-linted; that is stated in a comment where the loop is. Two tests: the index-level one in the workspace harness (edit A, `dependentsOf` names B, lint(B) changes), and one e2e test (open B, write A to disk, watcher fires, B's diagnostics update).

## Phase 0: prerequisites (½ day) -- done

Landed: the two grammar fixes with their parse assertions, `routine.ts`, `relatedInformation` on `LintDiagnostic` with the server mapping it to a URI, `test/format.ts` shared by the fixture harness (with a guard in the severity suite), and `run-all-tests.ts` skipping `test-files/workspace/`.

Tests first, all in existing suites:
- `run-parse-tests.ts`: `DO foo` target is the Path node itself; `DO foo IN bar.prg` sets `inSession` to a Path and leaves no `UnknownStatement`; `DO (x)` target is an Identifier.
- `run-ast-tests.ts` regenerates `DoStatement.inSession` to include `Path`.
- `run-fix-tests.ts` outline assertions unchanged after the helper moves.

Code:
- `foxpro.pegjs`: `target:(!(...) t:PathOrExpression { return t; })`; `IN` accepts `UnquotedPath / StringLiteral / Identifier`.
- `routine.ts`: `routineParameters(node: ProcedureStatement): string[]` moved from `outline.ts`.
- `rule.ts`: `LintDiagnostic.relatedInformation?: { file: string; range: Range; message: string }[]`; `server.ts` maps `file` to a URI when publishing. `run-all-tests.ts` `format()` prints each related entry on an indented continuation line so `.expected` files record it; `format` moves to `test/format.ts` so the workspace harness shares it.
- `run-all-tests.ts` skips `test-files/workspace/`.

Done when `bun run test` is green and `DoStatement.target` is a node.

## Phase 1: harness, index, plumbing, definition, symbols, hover (4 days)

### Tests first
- `test/run-workspace-tests.ts` (~150 lines): for every `test-files/workspace/<case>/`, optionally read `case.json` (`searchPath`, `rules`, `tier: 1|2`), build the index over the directory with `workspace.ts`, lint every `.prg` with `options.workspace`, diff against per-file `.expected` using the shared formatter, support `--update`. Below the fixture loop, `check()`-style assertions for the index API: `resolveRoutine` ordering, `resolveFile` extension defaulting and search order, `upsert` change sets, `dependentsOf`.
- Tier-1/tier-2 parity test over all of `test-files/` (in the same suite).
- `navigation.ts` unit tests in a new `test/run-navigation-tests.ts`: `definitionAt` on a `DO`, a call, an `#INCLUDE`, a `SET PROCEDURE`, same-file wins over workspace, a macro target returns nothing; `hoverAt` renders the signature and the comment block; workspace symbol query matching.
- First workspace fixtures: `workspace/basic/` (two files, one calls the other), `workspace/search-path/` (a routine found only via `case.json` searchPath), `workspace/procedure-file/` (a `SET PROCEDURE TO lib` making lib's routines win over a same-named one elsewhere).
- `bun run test` and `package.json` `test` script include the two new suites.

### Code
- `index.ts`, `workspace.ts` (crawl with `fs.promises`, yield every 200 files, honour exclude globs via a small matcher, no dependency), `routine.ts`, `navigation.ts` (`nodeAt` walks the tree for the innermost node containing the position; a word-at-cursor regex is the fallback when the token is a bare string like `DoFormStatement.target`).
- `linter.ts`: `LinterOptions.workspace?: { index: WorkspaceIndex; file: string }`; `ctx.workspace` is a per-file view `{ index, file, resolveRoutine(key), resolveFile(name, kind) }`, undefined when no index was given.
- Client: `synchronize.fileEvents = workspace.createFileSystemWatcher('**/*.{prg,mpr,spr,h}')`; workspace folders reach the server through `InitializeParams.workspaceFolders` already. Pass `initializationOptions: { storagePath: context.globalStorageUri.fsPath }` now so phase 3's cache has a home.
- Server: capabilities `definitionProvider`, `workspaceSymbolProvider`, `hoverProvider`; `onInitialized` reads the workspace settings and starts the crawl under `window/workDoneProgress` ("Indexing FoxPro workspace: n/m"); `onDidChangeWatchedFiles` upserts or removes and triggers the dependents re-lint; `validateAndSend` feeds the open document's live tree into the index via `extract()` so an unsaved edit is visible to other open files; `lint` is called with `options.workspace` when enabled.
- e2e: `client/testFixture/` gains a second file so one test covers go-to-definition across files over the wire.
- README gets an "Across the workspace" section and the three settings rows.

Done when: `bun run test` green, the extension opened on `test-files/watertight/` jumps between files, `Ctrl+T` lists routines, hover shows a signature, and the crawl over the real Watertight tree finishes with the editor responsive (tier 1 only; target under 1 s for 5k files, logged by a `test/lint-dir.ts` timing line).

Also in this phase because it falls out of the harness for free: `test/lint-dir.ts <dir> [--rule code]` builds an index over any directory and prints every finding. It is the corpus-run tool phase 4 depends on, and the same discipline `TODO.md` wants for the `SET EXACT` rule.

## Phase 2: the three rules and the macro guard (2½ days)

### Tests first
- `dynamic.ts` assertions in `run-navigation-tests.ts` (or a small block in the workspace suite): true for `MacroSubstitute`, a `Path` containing `&`, an `Identifier`/expression as a DO or DO FORM target (parenthesised form), a `MemberExpression` or `MacroSubstitute` callee, a `StringLiteral` containing `&`; false for a plain `Path`, `Identifier` callee, plain string.
- Fixtures under `test-files/diagnostics/` for the same-file half of `too-many-arguments` (the rule works without an index, so the existing harness covers it), and under `test-files/workspace/` for the cross-file cases:
  - `too-many-arguments/`: over-supply reports, under-supply is silent, `DO file.prg WITH` checks the file's main `LPARAMETERS`, duplicates across files use the largest arity, macro and method calls are silent, same-file definition beats a workspace one with fewer parameters.
  - `duplicate-routine/`: two files define `Foo`; each reports with `relatedInformation` naming the other; a method named like a routine is not a duplicate; `.mpr`/`.spr` count.
  - `missing-file/`: `#INCLUDE`, `SET PROCEDURE`, `SET CLASSLIB` (resolves `.vcx` via `known`), `DO dir\x.prg`, `DO FORM x` (`.scx`); resolved relative to the file, then a root, then `searchPath`; absolute path outside every root and `searchPath` is silent (the corpus's `S:\Libs\...` case); dynamic targets silent.
  - `stale-dependents/`: the index-level edit test described above.
- `run-severity-tests.ts` fails until `package.json` and the README name all three, as it does today.

### Code
- `dynamic.ts`, `rules/workspace.ts` holding the three rules, registered in `rules/index.ts`.
- `too-many-arguments` (warning): `onNode(['CallExpression', 'DoStatement'])`. Callee must be an `Identifier` or a `Path` naming a bare routine or a `.prg`; arguments counted including omitted (`f(1,,3)` passes 3). Definitions: this file's own `extract()` first, then `ctx.workspace.resolveRoutine`. If no candidate is found, stay silent (that is `unknown-routine`'s job). Report when every candidate at the winning resolution tier declares fewer parameters than passed. Message: `'Foo' takes 2 parameters, 3 passed`.
- `duplicate-routine` (warning): `onFile`; for each top-level routine in this file, other files' definitions of the same key become `relatedInformation`. Message: `'Foo' is also defined in lib.prg; FoxPro picks whichever is found first`.
- `missing-file` (warning): `onNode(['IncludeStatement', 'SetCommand', 'DoStatement', 'DoFormStatement'])`. `SetCommand` only for `PROCEDURE`/`CLASSLIB`. `DoStatement` only when the target carries a directory or extension; a bare `DO foo` is a routine reference and belongs to `unknown-routine`. Uses `resolveFile`; never touches `fs` at lint time, the crawler's `known` set is the truth.
- Server: the dependents re-lint loop, and `relatedInformation` published as LSP `DiagnosticRelatedInformation`.

Done when the three fixtures pass, `test/lint-dir.ts test-files/watertight` reports nothing for the three rules, and the README table has three new rows.

## Phase 3: tier 2, references, completion, signature help (3 days)

### Tests first
- Workspace suite: `references/` case asserting `referencesAt` over a directory finds `DO`, call, and `SET PROCEDURE` sites in three files and skips a `&` site.
- `workspace.ts` cache test: build, serialise to a temp dir, rebuild from cache with unchanged mtimes and assert no file was parsed (a counter on the parse hook); touch one file and assert exactly one parse.
- `run-navigation-tests.ts`: `completionsAt` lists routines and constants and, when the cursor follows `alias.`, the field names the file shows for that alias (reuse `collectFieldNames` from `rules/symbols.ts`, moved to `scope.ts`); `signatureAt` returns the parameter list and active index for `Foo(1, |)` and `DO Foo WITH 1, |`.

### Code
- `workspace.ts`: an idle queue (`setImmediate` between files, paused while any validation is pending) that promotes files to tier 2; records serialised to `<storagePath>/index-<hash of roots>.json` keyed by path with mtime and size, written debounced 5 s after the queue drains. Never ASTs.
- Server: `referencesProvider` (kicks the tier-2 queue to the front if incomplete and answers when it drains, under progress), `completionProvider` with `triggerCharacters: ['.']`, `signatureHelpProvider` with `['(', ',']`.
- `navigation.ts`: the three functions.

Done when find-all-references over the Watertight tree answers from cache on a restart without re-parsing.

## Phase 4: the risky rules, gated on a corpus run (3 days)

### Tests first
- `builtins.ts` parity: every name in the grammar's `KeywordFunction` list is in the table, and the table has no duplicates.
- Workspace fixtures: `unknown-routine/` (silent on a builtin, a variable in scope, a macro, a routine in a `SET PROCEDURE` file, a name defined in a `.vcx`-shaped class library the index cannot see; reports a plain typo), `class-checks/` (unknown base in `DEFINE CLASS x AS y` when y is neither a VFP base class nor indexed; `THIS.foo` where no indexed ancestor declares foo, silent when any ancestor is not indexed), `unused-routine/` (off by default; hint on a routine nothing references once tier 2 is complete), `undefined-constant/` (ALL_CAPS read, never assigned, not in this file's `#DEFINE`s or its transitive `#INCLUDE`s).

### Code
- `builtins.ts`: the VFP 9 function and command name table, ~600 entries, one name per line so diffs are readable.
- The four rules in `rules/workspace.ts`; `unknown-routine` and `unused-routine` ship `off`, `unused-routine` at hint severity, and the class checks report only when every class on the chain is indexed.
- Gate: `test/lint-dir.ts <real Watertight tree> --rule unknown-routine` run unsuppressed; the count and the reasons for each false positive go into `TODO.md` before the rule's default moves from `off`.

## Skipped, deliberately

- **Rename**: not implemented. Macro substitution makes a correct rename unprovable; the brief's LOCAL-only variant is not worth a provider that has to refuse most requests.
- **Cross-file diagnostics for closed files**: findings appear in the file they belong to when it is open, matching the publish model in place today. A workspace-wide Problems view would mean publishing for closed files and clearing on delete, which is a separate feature.
- **`.vcx`/`.scx` contents**: only their existence is indexed, for `missing-file`. Class checks stay silent whenever a chain leaves the `.prg` world.

## Totals

| Phase | Days | Ships |
|---|---|---|
| 0 | ½ | two grammar fixes, shared helpers, related-information plumbing |
| 1 | 4 | harness, index, watcher, progress, go to definition, workspace symbols, hover, `lint-dir` |
| 2 | 2½ | `too-many-arguments`, `duplicate-routine`, `missing-file`, `isDynamic`, dependents re-lint |
| 3 | 3 | tier 2, disk cache, references, completion, signature help |
| 4 | 3 | builtins table, `unknown-routine`, class checks, `unused-routine`, `undefined-constant` |

13 days, in line with the brief's 12–14. Phases 0 through 2 are the ones that pay for themselves at Watertight; 3 and 4 can each be shipped as a release of their own.
