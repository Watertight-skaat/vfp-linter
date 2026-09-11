* A ledger of constructs the grammar still does not read, recorded so the gap is stated out loud rather than passing silently. When one of these is implemented, `bun run test:update` removes its line and the diff shows coverage improving.
* Everything here announces itself as unsupported-syntax, which is the acceptable failure mode: a gap that reports itself costs one statement, while a statement that misparses into a valid tree costs every rule downstream and reports nothing at all.
* A line that leaves a *partial* node behind is marked as such: the statement before the remainder parsed into something, so half of it is already readable and only the tail announces itself.
* Each of these was found by probing the parser rather than by reading the grammar, which is the only way the list stays true.

* Referential integrity: the trigger a table fires on a change, and the check that the database container still matches what is on disk.
CREATE TRIGGER ON customer FOR INSERT AS NewCustomer()
DELETE TRIGGER ON customer FOR INSERT
VALIDATE DATABASE RECOVER

* SHUTDOWN ends the VFP session, running ON SHUTDOWN first. QUIT, which does not, is read.
SHUTDOWN

* The Foxbase menu system, which predates DEFINE POPUP and still turns up in the oldest files. MENU TO puts the chosen bar number in a variable, so what is lost here is a write the symbol table never sees.
MENU BAR mBar, 5
MENU TO lnChoice
READ MENU TO lnChoice

* RELEASE's screen forms. RELEASE reads as far as the word, then takes MENU for the name of a variable to release and leaves the real name behind. (partial)
RELEASE MENU mMain EXTENDED
RELEASE POPUP pFileMenu
