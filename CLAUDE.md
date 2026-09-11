This is a vscode Foxpro linter extension repository. The extension is available publicly, but mainly used by the Watertight internal team, so we cater to their codebase

Conventions:
- Be as concise as you can in your code and comments
- single-line JavaScript if/for statements don't need brackets
- Don't split up long comments to multiple lines. Modern IDEs have text wrap, so just leave them on one line

Notes:
- If `bun run dev` is already running, the parser is rebuilt on save and you do not need to rebuild it yourself.