* The bootstrap every data-access helper in the ERP opens with: a .prg whose whole job is to hand off
* to a Session object cached on _screen, creating it the first time anyone asks. TYPE() on a property
* path is how the app tests for "does this object exist yet", so the grammar sees a lot of it.
LPARAMETERS m.TableName, m.RecordId, m.lRefresh

LOCAL m.RetVal

IF TYPE("_screen.MosDA.RecordCache.DataSession") # "N"
	IF TYPE("_screen.MosDA.DataSession") # "N"
		_screen.AddObject("MosDA", "Session")
	ENDIF
	_screen.MosDA.AddProperty("RecordCache", CREATEOBJECT("RecordCache"))
ENDIF

m.RetVal = _screen.MosDA.RecordCache.Fetch(m.TableName, m.RecordId, m.lRefresh)

RETURN m.RetVal


DEFINE CLASS RecordCache AS Session

	DataSession	= 2
	Loaded		= .f.
	LastKey		= ""

	PROCEDURE Init
		DO DataSessionSetSettings
		USE files IN 0 ALIAS Cache_FILES ORDER filename
		USE fields IN 0 ALIAS Cache_FIELDS ORDER filefield	&& FILENAME+FIELD_NAME
	ENDPROC

	PROCEDURE Fetch
		LPARAMETERS m.TableName, m.RecordId, m.lRefresh
		LOCAL m.Trimmed, m.Found

		m.Trimmed = ALLTRIM(m.TableName)
		IF !USED(m.Trimmed)
			USE (m.Trimmed) AGAIN IN 0
		ENDIF

		m.Found = SEEK(PADR(m.RecordId, 8), m.Trimmed, "mosnum")
		IF !m.Found
			RETURN .null.
		ENDIF

		THIS.LastKey = m.RecordId
		THIS.Loaded = .t.
		RETURN THIS.LastKey
	ENDPROC

	PROCEDURE Release
		IF USED("Cache_FILES")
			USE IN Cache_FILES
		ENDIF
		RETURN .t.
	ENDPROC

ENDDEFINE
