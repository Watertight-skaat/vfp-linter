This probably doesn't cover ALL cases, especially as it gets to macro substitution. We support only the last month's worth of VSCode installs

## Settings

| Setting                              | Default         | What it does                                                   |
| ------------------------------------ | --------------- | -------------------------------------------------------------- |
| `foxpro.maxNumberOfProblems`         | `100`           | Caps the diagnostics reported per file                          |
| `foxpro.unsupportedSyntaxSeverity`   | `information`   | How to report statements the grammar cannot parse yet           |

`unsupportedSyntaxSeverity` exists because "the linter does not know this statement" is not the
same claim as "this statement is wrong". The grammar does not cover all of FoxPro yet, so valid
code can reach the catch-all rule, and reporting that as an error puts red squiggles under
working programs. It is advisory by default and can be set to `error`, `warning`, `hint` or `off`.

Two things are never downgraded with it, because both are genuinely wrong: a syntax error the
parser throws on, and a block whose terminator is missing (`unterminated-block`). The second
needs the special case because the grammar's catch-all swallows the opening line of an
unterminated `IF`, `FOR`, `TRY`, `WITH` or `DEFINE CLASS` instead of failing the parse, so
nothing else would report it.

## Developing the LSP

- Install [Bun](https://bun.sh), then run `bun install` in this folder. This installs the dependencies for the root, client and server packages.
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
| `bun install`       | Installs root, client and server dependencies                          |
| `bun run compile`   | Regenerates the parser, type-checks, and bundles client + server       |
| `bun run dev`       | Watches the grammar, the bundles and both type-check projects          |
| `bun run typecheck` | Type-checks only (esbuild does not type-check)                         |
| `bun run test`      | Parses every `.prg` in `test-files/`, then asserts the symbol table    |
| `bun run e2e`       | Launches VS Code and runs the end-to-end suite in `client/src/test`    |

> Use `bun run test`, not `bun test`. `bun test` is Bun's own test runner and ignores the
> `test` script -- it picks up the suites under `client/src/test`, which need a running
> VS Code host, and reports failures that mean nothing outside `bun run e2e`.

### Build output

`scripts/build.mjs` bundles `client/src/extension.ts` and `server/src/server.ts` with
esbuild into two self-contained CommonJS files. The packaged VSIX therefore contains no
`node_modules` at all -- only `client/out/extension.js`, `server/out/server.js` and the
manifests. `vscode` is the single external, because the extension host injects it.

Do not add `!` negation rules to `.vscodeignore`; they are applied last and will re-include
the whole of `node_modules`.

The parser suite runs straight from source (Bun imports `server/src/linter.ts` directly),
so `bun run test` needs no build step.

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
├── package.json // The extension manifest.
└── server // Language Server
    └── src
        ├── ast.ts // Shared node/location shapes for the Peggy AST
        ├── foxpro.pegjs // The grammar
        ├── linter.ts // Rules; importable without a connection, so tests can run it
        ├── scope.ts // Per-routine symbol table the scope-dependent rules read
        └── server.ts // Language Server entry point
```

### The symbol table

`scope.ts` turns the AST into one scope per routine: `(main)` for file-level code, one per
`PROCEDURE`/`FUNCTION`, one per `DEFINE CLASS` and one per method. Each scope records what it
declares (`LOCAL`, `PUBLIC`, `PRIVATE`, `LPARAMETERS`/`PARAMETERS`, `DIMENSION`, class
properties) with the declared type and site, every read and write of every name, and the
work-area changes in source order so `aliasInEffectAt()` can say which alias is current.

The rules that need it are not written yet -- unused `LOCAL`, the implicit `PRIVATE` created by
an undeclared assignment, a missing `m.` prefix on a name that is also a field, and work-area
handling all read this one structure. `test-files/run-scope-tests.js` asserts its contents
against `test-files/scope.prg`.

Two deliberate subtleties. A name that was never declared still gets an entry, with kind
`implicit` and no declaration site -- that is exactly the implicit-`PRIVATE` case, and it keeps
use-before-declare visible. And a bare name inside a SQL statement is marked `sqlContext`,
because it may be a column rather than a variable; rules should read that as ambiguous, enough
to call a local "used" but not enough to claim the variable was really touched.
