* An interchange parser, written as a class with the separators as properties so a trading partner
* with different delimiters is a subclass rather than a fork.
DEFINE CLASS X12Interchange AS Custom

	DataElementSeparator	= "*"
	ComponentElementSep		= ":"
	SegmentTerminator		= "~"
	cRawDocument			= ""
	nSegmentCount			= 0
	lStrict					= .f.

	PROCEDURE Init
		LPARAMETERS m.cDataSep, m.cCompSep, m.cSegTerm
		LOCAL m.nParentResult

		m.nParentResult = DODEFAULT(m.cDataSep, m.cCompSep, m.cSegTerm)
		IF !EMPTY(m.cDataSep)
			THIS.DataElementSeparator = m.cDataSep
		ENDIF
		IF !EMPTY(m.cCompSep)
			THIS.ComponentElementSep = m.cCompSep
		ENDIF
		IF !EMPTY(m.cSegTerm)
			THIS.SegmentTerminator = m.cSegTerm
		ENDIF
		RETURN .t.
	ENDPROC

	PROCEDURE Load
		LPARAMETERS m.cFileName
		LOCAL m.cText, m.i, m.nSegments, m.cSegment

		IF !FILE(m.cFileName)
			RETURN .f.
		ENDIF

		m.cText				= FILETOSTR(m.cFileName)
		THIS.cRawDocument	= m.cText
		m.nSegments			= OCCURS(THIS.SegmentTerminator, m.cText)
		THIS.nSegmentCount	= m.nSegments

		CREATE CURSOR curSegments (seq n(6), tag c(3), body m(4))

		FOR m.i = 1 TO m.nSegments
			m.cSegment = ALLTRIM(STREXTRACT(m.cText, "", THIS.SegmentTerminator, m.i, 1))
			IF EMPTY(m.cSegment)
				LOOP
			ENDIF
			INSERT INTO curSegments (seq, tag, body) ;
				VALUES (m.i, LEFT(m.cSegment, 3), m.cSegment)
		ENDFOR

		RETURN THIS.nSegmentCount
	ENDPROC

	FUNCTION Element(m.cSegment AS Character, m.nPosition AS Integer) AS Character
		LOCAL m.cValue
		m.cValue = GETWORDNUM(m.cSegment, m.nPosition + 1, THIS.DataElementSeparator)
		RETURN IIF(ISNULL(m.cValue), "", ALLTRIM(m.cValue))
	ENDFUNC

	PROCEDURE Error
		LPARAMETERS m.nError, m.cMethod, m.nLine
		IF THIS.lStrict
			THROW "X12 parse failed in " + m.cMethod + " at line " + TRANSFORM(m.nLine)
		ENDIF
		RETURN
	ENDPROC

ENDDEFINE
