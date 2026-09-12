* A ledger of constructs the grammar still does not read, recorded so the gap is stated out loud rather than passing silently. When one of these is implemented, `bun run test:update` removes its line and the diff shows coverage improving.
* Everything here announces itself as unsupported-syntax, which is the acceptable failure mode: a gap that reports itself costs one statement, while a statement that misparses into a valid tree costs every rule downstream and reports nothing at all.
* A line that leaves a *partial* node behind is marked as such: the statement before the remainder parsed into something, so half of it is already readable and only the tail announces itself.
* Each of these was found by probing the parser rather than by reading the grammar, which is the only way the list stays true.

* ON() reporting the current handler, in a file where ON is also a command word.
m.oError = ON("error")

* Whitespace between an alias and its dotted field. VFP allows it and the old report code uses it to line columns up, but it has to be told apart from the dot operators: `mastinfo .creditcard .or. mastinfo .ach` is two field reads and one operator.
m.lFlags = IIF(mastinfo .creditcard .or. mastinfo .ach, ",0", "")
