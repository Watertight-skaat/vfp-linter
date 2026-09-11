* Change history: instead of a datemod column, rows of audited tables are copied into a parallel log
* table under datalog\, fired from the close-item path. The old row is compared field by field.
LPARAMETERS m.cFileName, m.oFields, m.lNoCompare, m.cStat, m.cGroupMos

LOCAL m.cLogTable, m.cIdExpr, m.cItemId, m.nChanged, m.cFieldName, m.uOld, m.uNew
PRIVATE m.FieldArray

m.cFileName	= ALLTRIM(m.cFileName)
m.cLogTable	= "DataLogTable"
m.nChanged	= 0

IF !SEEK(PADR(UPPER(m.cFileName), 8), "files", "filename") .or. !files.datalog
	RETURN .f.
ENDIF

m.cIdExpr	= STRTRAN(ALLTRIM(files.mosidex), "(dbf)->", "")
m.cItemId	= EVALUATE(STRTRAN(ALLTRIM(files.mosidex), "(dbf)->", "m.oFields."))

TRY
	IF !USED(m.cLogTable)
		USE (m.m_tpath + "datalog\" + m.cFileName) AGAIN ALIAS DataLogTable IN 0
	ENDIF
CATCH
ENDTRY

IF !USED(m.cLogTable)
	IF !DIRECTORY("datalog")
		RETURN .f.
	ENDIF
	DO FORM doredict WITH mem_obj("auto_redict", "path", "data\", "message", "Create Data Log")
ENDIF

SELECT field_name, field_type, field_len ;
	FROM fields ;
	WHERE filename = PADR(UPPER(m.cFileName), 8) .and. !nolog ;
	ORDER BY field_name ;
	INTO CURSOR curLogFields NOFILTER

SCAN
	m.cFieldName = ALLTRIM(curLogFields.field_name)
	m.uNew = EVALUATE("m.oFields." + m.cFieldName)
	m.uOld = IIF(SEEK(m.cItemId, m.cLogTable, "mosnum"), EVALUATE(m.cLogTable + "." + m.cFieldName), m.uNew)
	IF m.lNoCompare .or. VARTYPE(m.uOld) # VARTYPE(m.uNew) .or. !(m.uOld == m.uNew)
		m.nChanged = m.nChanged + 1
		INSERT INTO (m.cLogTable) (log_mosnum, log_field, log_old, log_new, log_stat, log_when, log_user) ;
			VALUES (m.cItemId, PADR(m.cFieldName, 10), TRANSFORM(m.uOld), TRANSFORM(m.uNew), m.cStat, DATETIME(), m.sysunum)
	ENDIF
ENDSCAN

USE IN curLogFields
=FlushWrapper(.f., m.cLogTable)

RETURN m.nChanged
