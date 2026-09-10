This repo uses [Bun](https://bun.sh) as its package manager.

- Install with `bun install` (the root install also installs `client` and `server`).
- Run the parser test suite with `bun run test` -- **not** `bun test`, which is Bun's own
  test runner and ignores the `test` script. The parser suite runs from source; no build
  is needed for it.
- To rebuild after a grammar change: `bun run compile` (regenerates the parser, type-checks,
  then bundles with esbuild). `bun run e2e` runs the VS Code end-to-end suite.
- esbuild does not type-check. If you change types, run `bun run typecheck`.
- There is no ESLint in this repo: it is on TypeScript 7, which typescript-eslint does not
  support yet. Do not add lint scripts. `noUnusedLocals` / `noUnusedParameters` in
  `tsconfig.json` cover unused-code checks instead.
- If `bun run dev` is already running, the parser is rebuilt on save and you do not need to
  rebuild it yourself.
