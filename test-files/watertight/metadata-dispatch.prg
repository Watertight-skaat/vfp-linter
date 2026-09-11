* Menus and table actions are rows, not compiled objects: each row carries a command string that is
* macro-executed, so this file is mostly EVALUATE and & against text pulled out of a DBF.
LPARAMETERS m.cActionId, m.cTableName, m.oRecord

LOCAL m.cDoWhat, m.cExpression, m.cAlias, m.uResult

IF !USED("fileact")
	USE fileact IN 0 ORDER mosnum
ENDIF

IF !SEEK(PADR(m.cActionId, 8), "fileact", "mosnum")
	RETURN .null.
ENDIF

m.cDoWhat		= ALLTRIM(fileact.dowhat)
m.cExpression	= STRTRAN(ALLTRIM(fileact.idexpr), "(dbf)->", ALLTRIM(m.cTableName) + ".")
m.cAlias		= ALLTRIM(m.cTableName)

DO CASE
	CASE "do form" $ LOWER(m.cDoWhat)
		&cDoWhat
	CASE "runfileaction(" $ LOWER(m.cDoWhat)
		m.uResult = EVALUATE(m.cDoWhat)
	CASE LEFT(LOWER(m.cDoWhat), 3) == "do "
		&cDoWhat
	OTHERWISE
		m.uResult = EVALUATE(m.cExpression)
ENDCASE

* Menu rows are keyed by the same eight-character ids, and the prompts come from the row too.
SELECT mosnum, prompt, dowhat, seq ;
	FROM menuopt ;
	WHERE parentmos = PADR(m.cActionId, 8) .and. !inactive ;
	ORDER BY seq ;
	INTO CURSOR curMenu NOFILTER

SCAN
	IF EMPTY(curMenu.dowhat)
		LOOP
	ENDIF
	? ALLTRIM(curMenu.prompt), ALLTRIM(curMenu.dowhat)
ENDSCAN

ON KEY LABEL F1 DO helpd
ON KEY LABEL ESC &cOldEscHandler
ON KEY LABEL F9

RETURN m.uResult
