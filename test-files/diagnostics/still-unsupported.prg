* A ledger of constructs the grammar still does not read, recorded so the gap is stated out loud rather than passing silently. When one of these is implemented, `bun run test:update` removes its line and the diff shows coverage improving.
* Everything here announces itself as unsupported-syntax, which is the acceptable failure mode: a gap that reports itself costs one statement, while a statement that misparses into a valid tree costs every rule downstream and reports nothing at all.
* A line that leaves a *partial* node behind is marked as such: the statement before the remainder parsed into something, so half of it is already readable and only the tail announces itself.
* Each of these was found by probing the parser rather than by reading the grammar, which is the only way the list stays true.

* Transactions. These wrap the table buffering commands that are already read, so a rule about a write that is never committed has nothing to hang off until they are.
BEGIN TRANSACTION
END TRANSACTION
ROLLBACK

* The database container's other half: the commands that make one and open it. CREATE TRIGGER and VALIDATE DATABASE are read; what holds them is not.
CREATE DATABASE mydata
OPEN DATABASE mydata
CREATE CONNECTION myconn DATASOURCE 'dsn'
FREE TABLE customer
REMOVE TABLE customer

* DELETE's other objects, all three the same shape DELETE TRIGGER was: DELETE parses and the object clause after it is what is lost. (partial)
DELETE DATABASE mydata
DELETE VIEW myview
DELETE CONNECTION myconn

* Console input. Both put what was typed in the variable, so what is lost is a write the symbol table never sees -- the shape MENU TO had.
INPUT 'Name: ' TO lcName
ACCEPT 'Name: ' TO lcName

* The old READ screen, which @ ... GET fills. SHOW GETS, the other half of it, is read now.
READ CYCLE

* The @ ... EDIT control. The rest of @ is read, including the bare coordinates that only move the print head.
@ 3, 2 EDIT m.cUsed SIZE 17, 75 NOEDIT

* ON() reporting the current handler, in a file where ON is also a command word.
m.oError = ON("error")

* Bare ? with nothing to print, which emits a blank line.
?

* A form name containing a hyphen. This one does not announce the whole gap: DO FORM reads the name as far as the hyphen and only the remainder is reported, so the statement looks read and names the wrong form.
DO FORM start-up_code_mod

* Whitespace between an alias and its dotted field. VFP allows it and the old report code uses it to line columns up, but it has to be told apart from the dot operators: `mastinfo .creditcard .or. mastinfo .ach` is two field reads and one operator.
m.lFlags = IIF(mastinfo .creditcard .or. mastinfo .ach, ",0", "")
