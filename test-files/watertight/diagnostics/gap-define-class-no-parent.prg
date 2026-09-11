* DEFINE CLASS with no AS clause. VFP defaults the parent to Custom, and one-off helper classes written next to the program that uses them leave it off. The AS clause is required by the grammar, so the class opener is rejected and the file fails on the ENDDEFINE.
DEFINE CLASS ChangeExp

	DataSession = 2

	PROCEDURE Run
		LOCAL m.dNew
		m.dNew = GOMONTH(DATE(), 12)
		RETURN m.dNew
	ENDPROC

ENDDEFINE
