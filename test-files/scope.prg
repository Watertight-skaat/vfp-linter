* Fixture for the per-routine symbol table in server/src/scope.ts.
* run-scope-tests.js asserts against the routine and variable names below, so keep them in step.
LOCAL lcGreeting
lcGreeting = "hello"
USE customer
SELECT 0
USE orders IN 0 ALIAS ord

PROCEDURE CountRows
	LPARAMETERS tcAlias, tnStart
	LOCAL lnRows, lcUnused
	LOCAL ARRAY laBuffer(10)
	DIMENSION laGrid(2, 3)
	PRIVATE pnSeed
	PUBLIC gnTotal
	lnRows = tnStart
	lnUndeclared = lnRows + 1
	m.lnRows = m.lnRows + 1
	STORE 0 TO pnSeed, gnTotal
	FOR lnI = 1 TO 10
		laBuffer[lnI] = lnI
	ENDFOR
	SELECT cust_id FROM customer INTO CURSOR curTmp
	SELECT curTmp
	RETURN lnRows
ENDPROC

FUNCTION Describe(tcName AS Character)
	LOCAL lcOut AS Character
	PRIVATE pcHeading AS String
	PUBLIC gnAccountID AS Integer
	pcHeading = "<h1>"
	gnAccountID = 1
	lcOut = m.pcHeading + ALLTRIM(tcName) + TRANSFORM(m.gnAccountID)
	RETURN lcOut
ENDFUNC

DEFINE CLASS Widget AS Custom
	cName = ""
	nCount = 0

	PROCEDURE Init
		THIS.cName = "w"
	ENDPROC

	FUNCTION Label()
		LOCAL lcLabel
		lcLabel = THIS.cName
		RETURN lcLabel
	ENDFUNC
ENDDEFINE

FUNCTION Branching(tnType)
	LOCAL lcBranch
	DO CASE
	CASE tnType = 1
		lcBranch = "one"
	OTHERWISE
		lcBranch = "other"
	ENDCASE
	RETURN lcBranch
ENDFUNC

* SCAN and REPLACE used to drop their FOR and WHILE conditions, DIMENSION its column count and
* SET its argument, because each read the wrong index of a PEG sequence. Nothing reported it: the
* statements parsed, the conditions were just missing from the tree. These assertions fail again if
* that regresses.
PROCEDURE Filtering
	LPARAMETERS tnLimit, tnCols
	LOCAL lnSeen
	DIMENSION laWide(2, tnCols)
	lnSeen = 0
	SET MEMOWIDTH TO tnCols
	SCAN FOR invbal > tnLimit WHILE invbal < tnCols
		lnSeen = lnSeen + 1
	ENDSCAN
	REPLACE ALL invpaid WITH tnLimit FOR invbal > tnCols
ENDPROC

* Subscripts. Each line below used to parse into the wrong tree without reporting anything: the
* ARRAY keyword was read as a variable name, a parenthesised subscript split into a call plus a
* stray literal, and STORE matched the bare name and left the subscript behind. Only the symbol
* table shows the difference, which is why these are asserted here rather than as diagnostics.
PROCEDURE Subscripts
	LOCAL ARRAY laBrackets[2]
	PUBLIC ARRAY laPublic(2)
	PRIVATE ARRAY laPrivate[2, 3]
	laBrackets(1) = 10
	laBrackets[2] = 20
	STORE 0 TO laPublic[1]
	STORE 0 TO laPrivate(1, 2)
	STORE 0 TO laBrackets[1], laPublic[2], laPrivate[1, 3]
ENDPROC

* A dotted reference inside WITH names a property of the WITH target, not a memory variable. The
* grammar used to eat the leading dot, so every property assignment in every WITH block was booked
* here as a variable -- and would now be reported as an implicit PRIVATE. The arguments inside such a
* reference are still real reads, which is the part that makes this more than "skip the statement".
PROCEDURE Styling
	LPARAMETERS toGrid, tnColumn
	LOCAL lcHeading
	lcHeading = "Total"
	WITH m.toGrid
		.Caption = m.lcHeading
		.Columns(m.tnColumn).Width = 64
	ENDWITH
ENDPROC

* SCATTER and GATHER are how a record becomes an object, CATCH TO takes the m.-prefixed spelling house
* style puts on every variable, TEXT TO builds a string, and DO FORM's NAME and TO both create the name
* they are given. Every one of these is a reference the symbol table could not see at all: the
* statements either did not parse or parsed with the name thrown away, so nothing downstream knew the
* variable had been touched. A macro substitution is a read of the variable being run, which is the
* only thing standing between a macro-driven local and looking unused.
PROCEDURE Records
	LPARAMETERS tcTable
	LOCAL loRow, lcCommand, loErr, lcReport, loPicked
	lcCommand = "GO TOP"
	USE (m.tcTable) AGAIN IN 0
	SCATTER NAME m.loRow MEMO
	GATHER NAME m.loRow MEMO
	&lcCommand
	TEXT TO m.lcReport NOSHOW ADDITIVE TEXTMERGE PRETEXT 1
		<<m.loRow.invnum>>
	ENDTEXT
	DO FORM branchpick TO m.loPicked
	TRY
		COUNT TO lnSeen
	CATCH TO m.loErr
		? m.loErr.Message
	ENDTRY
	RETURN m.lcReport + m.loPicked
ENDPROC

* SCAN takes FOR and WHILE in either order. Reading them in a fixed order left the second clause to the
* catch-all, which reported it as an unterminated FOR block; here both operands have to reach the tree.
PROCEDURE Reordered
	LPARAMETERS tnLimit, tnCols
	SCAN REST WHILE invbal < tnCols FOR invbal > tnLimit
		? invbal
	ENDSCAN
ENDPROC

* A member named with a keyword is a property, not a memory variable. The names list is the assertion:
* TO, FROM and CLASS appearing here would mean the grammar had cut each reference at the dot and booked
* the keyword as a variable of its own.
PROCEDURE KeywordMembers
	LPARAMETERS toMessage
	LOCAL lcJoined
	lcJoined = m.toMessage.To + m.toMessage.From + m.toMessage.Class
	m.toMessage.To = m.lcJoined
ENDPROC

* A work area named by an expression. `USE IN (m.cAlias)` is how an area held in a variable is closed;
* the alias cannot be known statically, but the read of the variable naming it is still real.
PROCEDURE ComputedAreas
	LPARAMETERS tcAlias
	USE IN (m.tcAlias)
	SET ORDER TO (m.tcAlias) IN (m.tcAlias)
ENDPROC

* Both DELETE forms on the one node type. The SQL form's tables and joins hang off its FROM clause rather
* than sitting beside it on the statement, so the assertion is that the names in each clause are still
* reached: a FROM that stopped being visited would take the WHERE beside it out of the symbol table.
PROCEDURE Purging
	LPARAMETERS tnBatch
	LOCAL lnFloor
	lnFloor = 0
	DELETE FROM orders WHERE batch_id = m.tnBatch
	DELETE FOR qty < m.lnFloor IN orders
ENDPROC

* The Foxbase menu system, whose two halves are both references the symbol table used to miss: MENU BAR
* builds the bar from the array, which is a read of it, and MENU TO puts the number of the bar the user
* chose in the variable, which is a write. Neither statement parsed at all, so both names were invisible.
PROCEDURE LegacyMenu
	LOCAL lnChoice
	LOCAL ARRAY laBar(3)
	laBar(1) = "Post"
	MENU BAR laBar, 3
	MENU TO lnChoice
	RETURN m.lnChoice
ENDPROC

* Console input, the same shape MENU TO has: INPUT and ACCEPT both put what the user typed in the
* variable, which is a write, and the prompt beside it is an expression whose names are reads.
* lcCity is left undeclared on purpose: the implicit-private it earns is the proof that the write is seen.
PROCEDURE ConsoleInput
	LOCAL lcName, lcPrompt
	lcPrompt = "City: "
	INPUT "Name: " TO lcName
	ACCEPT m.lcPrompt TO lcCity
	RETURN m.lcName + m.lcCity
ENDPROC

* VFP's own system memory variables. _CUROBJ, _TALLY, _PAGENO and the rest of the underscore set exist
* before any code runs, so a write to one creates nothing and there is no declaration that would satisfy
* implicit-private -- LOCAL _curobj is a syntax error. _lcMine only looks like one: the underscore alone
* is not the test, so an undeclared name outside the set is still an implicit PRIVATE.
PROCEDURE SystemVariables
	LPARAMETERS tnObject
	_curobj = m.tnObject
	_pageno = 1
	_screen.Caption = "Watertight"
	_lcMine = _tally
	RETURN m.tnObject > 0 AND _tally = 0
ENDPROC
