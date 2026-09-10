This repo uses [Bun](https://bun.sh) as its package manager.

- Install with `bun install` (the root install also installs `client` and `server`).
- Run the parser test suite with `bun run test` -- **not** `bun test`, which is Bun's own
  test runner and ignores the `test` script.
- To rebuild after a grammar change: `bun run compile`.
- If `bun run dev` is already running, the parser is rebuilt on save and you do not need to
  rebuild it yourself.
