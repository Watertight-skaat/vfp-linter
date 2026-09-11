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

* Debugging and macro playback.
ASSERT lOk MESSAGE "no"
PLAY MACRO mMac

* The full-screen and console commands. WAIT is read in every spelling now, including the AT that follows the message; the three below are the control, and they must stay clean.
WAIT CLEAR
WAIT "" TIMEOUT 1
WAIT "NOT ON CASH ACCOUNTING" WINDOW
WAIT WINDOW "Saving" AT 10, 20 NOWAIT
ACTIVATE SCREEN
EJECT
READ EVENTS
RETRY
CANCEL
SHOW GETS
SHOW GET m.answer DISABLE

* @ with no clause after the coordinates, which just moves the print head, and the @ ... EDIT control.
@ PROW()+1, 1
@ 3, 2 EDIT m.cUsed SIZE 17, 75 NOEDIT

* AS on a declaration other than LOCAL. LOCAL takes a type and PRIVATE and PUBLIC do not, so the name is read and the type falls off; the OF clause naming the file a type is defined in is unread on all three.
PRIVATE cHeaderHTML as String
PUBLIC pnMailAccountID as Integer
LOCAL loSecurityAttributes as SECURITY_ATTRIBUTES OF oplocks.prg

* Data commands whose operand is an expression rather than a name, and the scope clause that names a record number.
DELETE RECORD RECNO("rec2inv") IN rec2inv
FLUSH IN (m.inWorkArea) FORCE
SET ORDER TO IIF(TYPE("m.cOrder") = "U", "servsnum", m.cOrder)
SET RELATION OFF INTO custinfo
MD (ADDBS(m.m_tpath) + "temp")
MODIFY COMMAND (m.cFile) NOWAIT
DIMEN invarr(1, 16)

* A second ADD COLUMN clause on ALTER TABLE. The first is read and the rest of the list is not, so the statement reports a gap that is really about everything after it.
ALTER TABLE items ADD COLUMN billcode c(6) ;
                  ADD COLUMN ledacct c(8)

* ADD OBJECT inside DEFINE CLASS, which is how a class declares a contained object rather than assigning one in Init.
DEFINE CLASS X12_Message AS Custom
	ADD OBJECT Segments as Collection
ENDDEFINE

* The optional THEN on IF, which the 2000s code writes and the rest does not. The IF itself is read, so the cost is a stray statement -- but it is a stray statement the symbol table books as a read of a variable named THEN.
IF m.stat = 3 THEN
ENDIF

* ON() reporting the current handler, in a file where ON is also a command word.
m.oError = ON("error")

* Bare ? with nothing to print, which emits a blank line.
?

* A form name containing a hyphen. This one does not announce the whole gap: DO FORM reads the name as far as the hyphen and only the remainder is reported, so the statement looks read and names the wrong form.
DO FORM start-up_code_mod

* Whitespace between an alias and its dotted field. VFP allows it and the old report code uses it to line columns up, but it has to be told apart from the dot operators: `mastinfo .creditcard .or. mastinfo .ach` is two field reads and one operator.
m.lFlags = IIF(mastinfo .creditcard .or. mastinfo .ach, ",0", "")

* A declared return type on a method. The AS clause is read on a parameter and on a LOCAL, and not here.
DEFINE CLASS Crypto AS Session
	FUNCTION Release AS Logical
		RETURN .t.
	ENDFUNC
ENDDEFINE
