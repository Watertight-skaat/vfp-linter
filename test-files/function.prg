* FUNCTION with an argument list closed by ENDFUNC
FUNCTION GetName(tcId)
	LOCAL lcResult
	lcResult = "x"
	RETURN lcResult
ENDFUNC

* empty argument list closed by ENDFUNC
FUNCTION Ping()
	RETURN .T.
ENDFUNC

* no argument list
FUNCTION Bare
	RETURN .F.
ENDFUNC

* ENDPROC still terminates a FUNCTION
FUNCTION Legacy()
	RETURN 1
ENDPROC

PROCEDURE DoIt
	PARAMETERS tcA, tcB
	? tcA
ENDPROC
