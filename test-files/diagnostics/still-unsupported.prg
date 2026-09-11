* A ledger of constructs the grammar still does not read, recorded so the gap is stated out loud rather than passing silently. When one of these is implemented, `bun run test:update` removes its line and the diff shows coverage improving.
* Everything here announces itself as unsupported-syntax, which is the acceptable failure mode: a gap that reports itself costs one statement, while a statement that misparses into a valid tree costs every rule downstream and reports nothing at all.
* A line that leaves a *partial* node behind is marked as such: the statement before the remainder parsed into something, so half of it is already readable and only the tail announces itself.
* Each of these was found by probing the parser rather than by reading the grammar, which is the only way the list stays true.

* The screen buffer saved to and restored from a variable. SAVE TO and RESTORE FROM, which do the same for memory variables, are read; SAVE WINDOW and RESTORE WINDOW are too. These two are the pair in between.
SAVE SCREEN TO lcScreen
RESTORE SCREEN FROM lcScreen

* SET MARK OF puts a tick beside a menu item. The OF form reads as far as the item, so only the TO clause is lost -- SET SKIP OF, which greys one out, is read in full. (partial)
SET MARK OF BAR 1 OF pFileMenu TO .T.

* An index file named with its extension in an OF clause. The name alone reads, so the tag is already known and only the file is lost: the expression reader meets `cust.cdx` and takes it for member access. (partial)
INDEX ON custid TAG custid OF cust.cdx ADDITIVE
