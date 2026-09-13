* A ledger of constructs the grammar still does not read, recorded so the gap is stated out loud rather than passing silently. When one of these is implemented, `bun run test:update` removes its line and the diff shows coverage improving.
* Everything here announces itself as unsupported-syntax, which is the acceptable failure mode: a gap that reports itself costs one statement, while a statement that misparses into a valid tree costs every rule downstream and reports nothing at all.
* A line that leaves a *partial* node behind is marked as such: the statement before the remainder parsed into something, so half of it is already readable and only the tail announces itself.
* Each of these was found by probing the parser rather than by reading the grammar, which is the only way the list stays true.

* ON() reporting the current handler, in a file where ON is also a command word.
m.oError = ON("error")

* Whitespace between an alias and its dotted field. VFP allows it and the old report code uses it to line columns up, but it has to be told apart from the dot operators: `mastinfo .creditcard .or. mastinfo .ach` is two field reads and one operator.
m.lFlags = IIF(mastinfo .creditcard .or. mastinfo .ach, ",0", "")

* #REGION / #ENDREGION, the folding markers VFP Advanced and FoxBin2Prg write into generated code. 754 of them in the Watertight tree, which makes this the single most common thing the grammar does not read.
#REGION 1
#ENDREGION

* REGIONAL, the declaration that scopes a name to the macro-expanded block rather than the routine. Nothing else declares a variable this way, so every name on the line is then reported as an implicit private: one unread statement costs three findings.
REGIONAL m.currarea, m.talkstat, m.compstat

* DOEVENTS, which yields to the message pump. A bare command word with an optional FORCE.
DOEVENTS

* ERROR, which raises one. Takes a number or a message string, and is how the update packager simulates a failure.
ERROR 1799

* A member reference standing alone as a statement inside WITH. `.SetFocus()` parses because the call makes it an expression; `.Click` with no argument list has nothing to make it one.
WITH _screen
	.Click
ENDWITH
