* Per-branch and per-user settings live in a preference table rather than in columns or an ini file,
* so every read is a seek with a typed fallback and every write is a locked replace.
LPARAMETERS m.cKey, m.uDefault, m.cStnum, m.cUser

LOCAL m.cScope, m.uValue, m.cType

m.cStnum	= IIF(TYPE("m.cStnum") = "C" .and. !EMPTY(m.cStnum), m.cStnum, m.sysstnum)
m.cUser		= IIF(TYPE("m.cUser") = "C", m.cUser, m.sysunum)
m.cScope	= PADR(m.cStnum, 5) + PADR(m.cKey, 30)
m.cType		= VARTYPE(m.uDefault)

IF !USED("branchpref")
	USE branchpref IN 0 ORDER stkey
ENDIF

IF !SEEK(m.cScope, "branchpref", "stkey")
	RETURN m.uDefault
ENDIF

DO CASE
	CASE m.cType = "C"
		m.uValue = ALLTRIM(branchpref.cval)
	CASE m.cType = "N"
		m.uValue = branchpref.nval
	CASE m.cType = "L"
		m.uValue = branchpref.lval
	CASE m.cType = "D"
		m.uValue = branchpref.dval
	CASE m.cType = "T"
		m.uValue = branchpref.tval
	OTHERWISE
		m.uValue = m.uDefault
ENDCASE

IF EMPTY(m.uValue) .and. !EMPTY(m.uDefault)
	m.uValue = m.uDefault
ENDIF

RETURN m.uValue


FUNCTION BranchPrefStore(m.cKey AS Character, m.uValue, m.cStnum AS Character) AS Logical
	LOCAL m.cScope

	m.cScope = PADR(m.cStnum, 5) + PADR(m.cKey, 30)
	IF !SEEK(m.cScope, "branchpref", "stkey")
		INSERT INTO branchpref (stnum, prefkey) VALUES (PADR(m.cStnum, 5), PADR(m.cKey, 30))
	ENDIF

	DO WaitForRlock IN Procfile WITH "branchpref", "Saving " + ALLTRIM(m.cKey)
	DO CASE
		CASE VARTYPE(m.uValue) = "C"
			REPLACE cval WITH m.uValue IN branchpref
		CASE VARTYPE(m.uValue) = "N"
			REPLACE nval WITH m.uValue IN branchpref
		CASE VARTYPE(m.uValue) = "L"
			REPLACE lval WITH m.uValue IN branchpref
		OTHERWISE
			REPLACE cval WITH TRANSFORM(m.uValue) IN branchpref
	ENDCASE
	=FlushWrapper(.f., "branchpref")
	UNLOCK IN branchpref

	RETURN .t.
ENDFUNC
