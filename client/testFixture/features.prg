* Fixture for the end-to-end feature tests: the Outline, folding ranges and the quick fixes.
PROCEDURE Alpha
	LPARAMETERS tcName
	IF EMPTY(tcName)
		lnCount = 1
	ENDIF
ENDPROC

DEFINE CLASS Widget AS Custom
	cName = ""

	PROCEDURE Init
		THIS.cName = "x"
	ENDPROC
ENDDEFINE
