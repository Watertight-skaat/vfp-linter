# Visual FoxPro Linter

Diagnostics for Visual FoxPro `.prg`, `.mpr` and `.spr` files in VS Code.

It parses your code with a real grammar rather than matching patterns, which is what lets the checks
below tell a field from a variable, a subquery from a statement, and syntax the linter has not learned
from code that is actually wrong.

## What it checks

| Code | Severity | What it reports |
| ---- | -------- | --------------- |
| `syntax-error` | Error | The parser could not read the file at all |
| `unterminated-block` | Error | A block opener whose terminator is missing |
| `unsupported-syntax` | Information | Valid FoxPro the grammar has not learned yet (configurable) |
| `missing-memvar-prefix` | Warning | A variable referenced without `m.` whose name is also used as a field |
| `unreachable-code` | Warning | A statement after `RETURN` / `EXIT` / `LOOP` in the same block |
| `duplicate-case` | Warning | A `CASE` condition identical to an earlier one in the same `DO CASE` |
| `private-all` | Warning | `PRIVATE ALL`, which hides every variable of the caller |
| `unlinked-tables` | Warning | Tables in `FROM` with nothing relating them: a Cartesian product |
| `select-without-into` | Warning | A query with no `INTO` or `TO`, which browses its whole result at run time |
| `having-without-group-by` | Information | `HAVING` with no `GROUP BY`, so it is only a post-filter |
| `try-without-catch` | Information | A `TRY` with neither `CATCH` nor `FINALLY` |
| `empty-branch` | Information | An `IF`, `ELSE`, `CASE` or `OTHERWISE` branch with no statements |

Every diagnostic carries its code, so you can filter or turn off any of them from the Problems panel.

### The ones that hold back on purpose

A linter that cries wolf gets switched off, so three of these are built to stay quiet unless there is
real evidence.

**`missing-memvar-prefix`.** When a memory variable and a field of an open table share a name, a bare
reference resolves to *the field* — so `lcName = "x"` can update the record instead of the variable.
Which names are fields cannot be known without opening the table, and flagging every bare reference
while a table is open would flag nearly every line of real FoxPro. So it reports only names your file
itself shows being used as a field: a column in a `CREATE`, a `REPLACE` target, an `INSERT` column
list, or a reference qualified by an alias the file opens. No evidence of a collision, no report.

**`unlinked-tables`.** Each table in `FROM` is a node and each condition mentioning two of them is an
edge; it reports when the graph comes out in more than one piece, and names the pieces. Three things
silence it, because any of them could be the missing link: a name the query cannot attribute to a
table (`WHERE cust_id = o_cust_id`), a macro, and a derived table. Two tables compared to the same
variable count as related, since that is how a parent and its children are usually fetched by a key.

**`empty-branch`** is advisory because comments are not part of the syntax tree, so a branch holding
only a comment reads as empty. An empty `ELSE` is reported at the `IF` line, which is the nearest
position the parser gives for it.

## Settings

| Setting | Default | What it does |
| ------- | ------- | ------------ |
| `foxpro.unsupportedSyntaxSeverity` | `information` | How to report statements the grammar cannot parse yet |
| `foxpro.maxNumberOfProblems` | `100` | Caps the diagnostics reported per file |

`unsupportedSyntaxSeverity` exists because "the linter does not know this statement" is not the same
claim as "this statement is wrong". The grammar does not cover all of FoxPro, so valid code can reach
the catch-all rule, and reporting that as an error puts red squiggles under working programs. It is
advisory by default and takes `error`, `warning`, `information`, `hint` or `off`.

Two things are never quieted with it, because both are genuinely wrong: a syntax error the parser
throws on, and a block whose terminator is missing. The second needs the special case because the
catch-all swallows the opening line of an unterminated `IF`, `FOR`, `TRY`, `WITH`, `DEFINE CLASS` or
`TEXT` rather than failing the parse, so nothing else would report it.

## What it does not read yet

Most of the language parses, including the parts that are easy to get wrong: `TEXT ... ENDTEXT` with
its body left as raw text, the `ON ERROR` family, SQL `CASE WHEN`, the `::` scope-resolution operator,
`@ ... SAY`/`GET`, and the Xbase housekeeping and output commands.

Still unread, and so reported as `unsupported-syntax`: `SCATTER`/`GATHER MEMVAR`, `MODIFY STRUCTURE`,
`COUNT`, `AVERAGE`, `FLUSH`, `PUSH`/`POP KEY`, `EXTERNAL`, `RUN`, and the menu commands. Macro
substitution is parsed where it appears, but a macro's contents are only known at run time, so checks
that depend on reading a condition step aside when they meet one.

If you hit something valid that reports as unsupported, an issue with the statement in it is the most
useful thing you can send.

## Requirements

VS Code 1.101 or later.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for the build, the test suites, and how the grammar, the typed
AST and the symbol table fit together.
