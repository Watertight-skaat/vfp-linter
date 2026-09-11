* A ledger of constructs the grammar still does not read, recorded so the gap is stated out loud rather than passing silently. When one of these is implemented, `bun run test:update` removes its line and the diff shows coverage improving.
* Everything here announces itself as unsupported-syntax, which is the acceptable failure mode: a gap that reports itself costs one statement, while a statement that misparses into a valid tree costs every rule downstream and reports nothing at all.

* Multiple STORE targets where one is subscripted. The single-target forms are read; only the list is not, and the remainder announces itself rather than dropping the subscript silently.
STORE 0 TO laRows[1], laCols[2]

* Table-level constraints in CREATE TABLE. Column-level UNIQUE, CHECK and REFERENCES are read; a constraint written after the column list is not, and it costs the whole CREATE TABLE its parse.
CREATE TABLE t (a C(5), UNIQUE a TAG at)
CREATE TABLE t2 (a C(5), FOREIGN KEY a TAG at REFERENCES p TAG pt)

* The screen and menu commands. A 30-year-old application still carries them, but none of them touches data or variables, so they cost a rule nothing.
DEFINE WINDOW wOut FROM 1,1 TO 10,40 TITLE "Out"
ACTIVATE WINDOW wOut
DEACTIVATE WINDOW wOut
MOVE WINDOW wOut TO 2, 2
DEFINE MENU mMain BAR
DEFINE BAR 1 OF mPop PROMPT "Go"
SET SKIP OF BAR 1 OF mPop .t.
ON SELECTION BAR 1 OF mPop DO Handler

* The pre-SQL data commands, which do have operands a rule would want: each names a table or a variable.
TOTAL ON stnum TO summary
JOIN WITH b TO c FOR a.k = b.k
UPDATE ON stnum FROM other REPLACE invbal WITH other.invbal
COPY STRUCTURE TO newtbl
DELETE TAG stnum
BLANK FIELDS invbal

* Memory-variable files and the remaining PRIVATE form.
SAVE TO mem ALL LIKE m_*
RESTORE FROM mem ADDITIVE
PRIVATE ALL EXCEPT m_*

* Debugging and macro playback.
ASSERT lOk MESSAGE "no"
PLAY MACRO mMac
