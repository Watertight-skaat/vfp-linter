# Workspace-level features: brief

Input for a planning agent. Goal: add cross-file intelligence to the VFP linter — editor navigation across the codebase, and rules that catch bugs a single-file linter cannot see.

## Where the code is now

Single-document only. Nothing in the server knows another file exists.

- `server/src/server.ts` — LSP entry. Holds `documents` (open only) and a `trees` map of the last AST per open document. Provides diagnostics, document symbols, folding ranges, code actions. Diagnostics are debounced 300 ms per document.
- `server/src/linter.ts` — `lint(text, options)`: parse, run rules, resolve severities, apply suppression comments, sort. Takes a string and returns diagnostics + AST. No file or workspace parameter.
- `server/src/ast.ts` — typed discriminated union over ~101 node types, re-derived from the grammar by `test/run-ast-tests.ts`. Relevant nodes already exist: `DoStatement`, `CallExpression`, `DoFormStatement`, `IncludeStatement`, `DefineStatement` (#DEFINE), `DefineClass`, `ProcedureStatement`, `ParametersDeclaration`, `SetCommand` (carries `command` + `arguments`, so `SET PROCEDURE TO` / `SET CLASSLIB TO` are readable).
- `server/src/scope.ts` — per-routine symbol table: declarations, reads/writes, work-area events, `openAliases`. Per file.
- `server/src/rules/` — 15 rules in 3 modules, registered in `rules/index.ts`. A rule is `onNode()` or `onFile()` and reports via `ctx.report(loc, message, fix?)`.
- `server/src/outline.ts` — already resolves a routine's parameters from either the header or an `LPARAMETERS` first statement. Reuse that logic for arity.
- `client/src/extension.ts` — 41 lines, no file watcher, no workspace folders passed.
- `test/` — 7 suites, all driving `lint(text)` with a single string. `test-files/` holds `.prg` fixtures with recorded `.expected` diagnostics.

## Conventions that constrain the work

- Tests first. New rules and grammar get their fixture before the implementation.
- Very high test coverage is expected; regressions are the thing being guarded against.
- Adding a rule means four edits: the rule module, `rules/index.ts`, the `foxpro.rules` schema in `package.json`, and the README table. `test/run-severity-tests.ts` fails until the last two are done.
- Code style: concise; single-line `if`/`for` need no brackets; long comments stay on one line.
- Nothing under `server/src` except `server.ts` may import the language server — the tests drive `lint()` directly.

## The foundation everything depends on

A **workspace index** (`server/src/index.ts`). Every feature below needs it; nothing below is worth planning separately from it.

Per file, it should record: routines (name, isFunction, parameter names/arity, location, owning class), classes (name, base, methods, properties), `#DEFINE` constants and values, outbound references (DO targets, call callees, DO FORM, #INCLUDE, SET PROCEDURE/CLASSLIB, CREATEOBJECT/NEWOBJECT string literals), and mtime + size for invalidation.

Two tiers, because a full parse of the real Watertight tree is too slow to do eagerly (a 27k-line file parses in ~250 ms):

- **Tier 1, header scan** — regex for `^\s*(PROCEDURE|FUNC|DEFINE CLASS|#DEFINE|LPARAMETERS)`. Microseconds per file, runs over the whole workspace at startup. Enough for definitions, workspace symbols, duplicate detection, arity checks.
- **Tier 2, full parse** — lazy, background, idle-queued. Needed only for reference finding. Cache extracted records (never ASTs) to disk keyed by mtime so restarts are instant.

Client-side prerequisites: pass workspace folders, add a `**/*.{prg,mpr,spr,h}` file watcher to `clientOptions.synchronize.fileEvents`, handle `onDidChangeWatchedFiles` on the server, and report the initial crawl via `window/workDoneProgress`.

New settings needed: `foxpro.workspace.enabled`, `foxpro.workspace.exclude` (globs), `foxpro.workspace.searchPath` (mimics VFP's `SET PATH` for file resolution).

## Test harness prerequisite

The existing suites cannot express a cross-file case. Before any of this: `lint()` needs to accept an index, and a new `test/run-workspace-tests.ts` needs a directory-shaped fixture — `test-files/workspace/<case>/*.prg` with per-file `.expected` — that builds an index over the directory and diffs. Roughly 150 lines. Everything else is blocked on it.

## Editor features (small once the index exists)

| Feature | Notes |
|---|---|
| Go to definition | `DO x`, `x()`, `#INCLUDE`, `SET PROCEDURE TO`. Same file first, then workspace. |
| Workspace symbols | Straight off tier 1. |
| Hover | Signature, params, and the comment block above the routine (VFP's de-facto docstring). `#DEFINE` values too. |
| Find all references | Requires tier 2 across the tree. |
| Completion | Routine names, constants; bonus: field names for an alias the file opens (`scope.ts` has `openAliases`). |
| Signature help | Arity and parameter names while typing arguments. |

**Rename should be skipped**, or restricted to `LOCAL`s within one routine and refused when the routine contains any `&` macro. Runtime-assembled names make correct rename unprovable in VFP.

## Cross-file rules, ranked by value-to-noise

1. **`too-many-arguments`** — a call passing more arguments than the routine declares (VFP throws at runtime). Passing fewer is legal, so only over-supply is reported. Near-zero false positives; best ratio in the list.
2. **`duplicate-routine`** — the same `PROCEDURE` name in two indexed files. VFP silently resolves by search order. Report both sites with `relatedInformation`.
3. **`missing-file`** — `#INCLUDE`, `SET PROCEDURE TO`, `SET CLASSLIB TO`, `DO x.prg`, `DO FORM x` whose target does not exist. Resolve relative to the file, then workspace roots, then `searchPath`.
4. **`unknown-routine`** — `DO foo` / `foo()` resolving to nothing. Highest value, highest risk. Needs a static table of several hundred VFP builtins, which does not exist in the repo today. Must stay silent on names that are also variables in scope, on macro targets, and on files loaded by `SET PROCEDURE`. Should not ship until run unsuppressed over the real corpus — the same discipline `TODO.md` applies to the `SET EXACT` rule.
5. **`undefined-constant`** — an ALL_CAPS name read but never assigned and never `#DEFINE`d in this file or its transitive includes. Depends on the include resolution built for #3.
6. **Class-aware checks** — unknown base class in `DEFINE CLASS x AS y`; `THIS.foo` on no ancestor. Classes living in `.vcx`/`.scx` are invisible to a `.prg` index, so this needs gating.
7. **`unused-routine`** — ship `off` by default at hint severity. Form and report call sites are not in `.prg` files at all.

## Two hazards the plan must address explicitly

**Macro substitution.** One shared `isDynamic(node)` helper — true for anything involving `&`, a macro, or a parenthesised runtime target — that every cross-file rule consults before reporting. One function, tested once, rather than each rule re-deriving the same evasion.

**Stale cross-file diagnostics.** If file A's routine is renamed, the finding belongs in file B. The index needs a reverse map (name → referencing files) and a debounced, capped re-lint of dependents when A changes. Easy to get wrong, hard to notice; it needs its own workspace-harness test case.

## Suggested phasing

1. Workspace test harness, index, go-to-definition, workspace symbols, hover.
2. `too-many-arguments`, `duplicate-routine`, `missing-file`.
3. Find all references, completion, signature help.
4. `unknown-routine` (after a corpus run), class checks, `unused-routine` opt-in.

Rough total: 12–14 days of work, front-loaded into phase 1.
