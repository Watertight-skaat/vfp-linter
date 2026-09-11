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

* DELETE's other objects, all three the same shape DELETE TRIGGER was: DELETE parses and the name is what is lost. (partial)
DELETE DATABASE mydata
DELETE VIEW myview
DELETE CONNECTION myconn

* Console input. Both put what was typed in the variable, so what is lost is a write the symbol table never sees -- the shape MENU TO had.
INPUT 'Name: ' TO lcName
ACCEPT 'Name: ' TO lcName

* The old READ screen, which @ ... GET fills and these two drive.
READ CYCLE
SHOW GETS

* Moving data in and out of the session, and the print job that wraps a report.
IMPORT FROM sales.xls TYPE XLS
EXPORT TO sales TYPE XLS
TYPE readme.txt
EJECT
PRINTJOB
ENDPRINTJOB
EDIT

* REGIONAL declares a variable local to the routine and to any macro it expands, so it is a declaration the symbol table never sees.
REGIONAL lcTemp

* The keyboard macro set.
SAVE MACROS TO mykeys.fky
RESTORE MACROS FROM mykeys.fky

* TOTAL's documented argument order. The reverse -- TOTAL ON key TO file -- is read, so this is a gap in a rule that already exists rather than a missing one.
TOTAL TO totals ON custid

* Three SETs whose argument runs past what the setting reader claims, each leaving the tail behind. (partial)
SET TOPIC ID TO 5
SET NOTIFY CURSOR OFF
SET WINDOW OF MEMO notes TO myform
