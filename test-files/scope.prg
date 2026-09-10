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
