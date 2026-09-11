* A ledger of constructs the grammar still does not read, recorded so the gap is stated out loud rather than passing silently. When one of these is implemented, `bun run test:update` removes its line and the diff shows coverage improving.
* Everything here announces itself as unsupported-syntax, which is the acceptable failure mode: a gap that reports itself costs one statement, while a statement that misparses into a valid tree costs every rule downstream and reports nothing at all.
* A line that leaves a *partial* node behind is marked as such: the statement before the remainder parsed into something, so half of it is already readable and only the tail announces itself.

* SET commands whose argument is a list or has a clause of its own. The bare `SET x TO y` form reads them, so each leaves a SetCommand behind and only the tail is lost.
SET SKIP TO custid INTO orders
SET RELATION OFF INTO orders
SET PROCEDURE TO lib1, lib2 ADDITIVE
SET CLASSLIB TO mylib IN app ALIAS al

* BROWSE options past the first. The statement is recognised, so the work area is known; the field list is not.
BROWSE FIELDS custid, name NOEDIT

* Window definitions saved to and read back from a file. The screen commands proper are read; these two are not.
SAVE WINDOW wOut TO layout
RESTORE WINDOW wOut FROM layout

* The rest, each costing only its own statement.
INSERT BEFORE BLANK
FIND smith
COPY INDEXES all
CREATE VIEW myview AS SELECT custid FROM orders
DECLARE laArr[3]
WAIT "" TO lcKey
DEBUGOUT lcMessage
