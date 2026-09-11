* PROTECTED and HIDDEN in front of a PROCEDURE or FUNCTION inside DEFINE CLASS. The bare forms are read and the scoped ones are not, so the method is rejected, and with it the whole class: the file fails on the ENDPROC. The framework classes mark nearly every internal method this way, which is why several of the largest files in the source parse not at all rather than partly.
* The property-list forms -- `PROTECTED hProv, hHash` on its own line -- are a smaller gap in the same place: they announce themselves as unsupported and cost only their own line.
DEFINE CLASS Crypto AS Session

	PROTECTED hProv, hHash

	hProv			= 0
	ErrorMessage	= ""

	PROTECTED PROCEDURE Init
		this.hProv = 0
	ENDPROC

	HIDDEN FUNCTION Release AS Logical
		this.hProv = 0
		RETURN .t.
	ENDFUNC

ENDDEFINE
