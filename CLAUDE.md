This is a vscode Foxpro linter extension repository. The extension is available publicly. The linter works, but doesn't currently support all Foxpro syntax (yet), and useful linting rules are a work-in-progress.

Conventions:
- Be as concise as you can in your code and comments
- Comments should be on one line (modern IDEs have text wrap)
- single-line JavaScript if/for statements don't need brackets

Notes:
- If `bun run dev` is already running, the parser is rebuilt on save and you do not need to
  rebuild it yourself.