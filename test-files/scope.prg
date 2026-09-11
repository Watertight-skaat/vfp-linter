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
	lcOut = ALLTRIM(tcName)
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
