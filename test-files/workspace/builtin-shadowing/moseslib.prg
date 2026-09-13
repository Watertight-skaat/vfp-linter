* A routine whose name is a VFP built-in function. The framework has carried FUNCTION VARTYPE since the 1990s, and legacy275 has PROCEDURE DATETIME and PROCEDURE error beside it. None of them takes a parameter.
* FoxPro resolves an intrinsic function before it looks for a user routine, so none of these is ever what a `VARTYPE(m.x)` call reaches. They are dead weight in the tree, not a routine the rest of the code calls.
FUNCTION VARTYPE
	RETURN "U"
ENDFUNC

PROCEDURE DATETIME
	? "prompts for the system clock"
ENDPROC

PROCEDURE error
	=MESSAGEBOX(MESSAGE())
ENDPROC
