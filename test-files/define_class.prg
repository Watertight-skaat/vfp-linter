DEFINE CLASS MyThing AS Custom
	cName = ""
	nCount = 0

	PROCEDURE Init
		THIS.cName = "hello"
	ENDPROC

	FUNCTION GetName()
		RETURN THIS.cName
	ENDFUNC

	FUNCTION SetName(tcName)
		THIS.cName = tcName
		RETURN .T.
	ENDFUNC
ENDDEFINE

DEFINE CLASS X12_Message AS Custom
	ADD OBJECT Segments as Collection
	ADD OBJECT oHeader AS Line WITH Caption = "ISA", Visible = .T.
ENDDEFINE

DEFINE CLASS Svc AS Session OLEPUBLIC
	FUNCTION Ping()
		RETURN .T.
	ENDFUNC
ENDDEFINE
