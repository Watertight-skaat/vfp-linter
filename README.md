This probably doesn't cover ALL cases, especially as it gets to macro substitution.

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

| Command | What it does |
| --- | --- |
| `bun install` | Installs root, client and server dependencies |
| `bun run compile` | Regenerates the parser, type-checks, and copies build output |
| `bun run dev` | Watches the grammar and TypeScript sources |
| `bun run lint` | ESLint over `client/src`, `server/src`, `scripts` and `test-files` |
| `bun run test` | Parses every `.prg` in `test-files/` and fails on any error diagnostic |

> Use `bun run test`, not `bun test`. `bun test` is Bun's own test runner and ignores the
> `test` script -- it picks up the unused VS Code end-to-end fixtures under `client/src/test`
> and reports failures that mean nothing.

The client, server and root each keep their own `node_modules`. This is deliberate: the
extension is packaged by allowlisting the client's runtime dependencies out of
`client/node_modules` in `.vscodeignore`, and a hoisting workspace install leaves that
directory incomplete -- producing a VSIX that installs but cannot activate.

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