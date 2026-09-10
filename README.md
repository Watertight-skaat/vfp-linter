This probably doesn't cover ALL cases, especially as it gets to macro substitution. We support only the last month's worth of VSCode installs

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
| `bun run test`      | Parses every `.prg` in `test-files/` and fails on any error diagnostic |
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
        └── server.ts // Language Server entry point
```
