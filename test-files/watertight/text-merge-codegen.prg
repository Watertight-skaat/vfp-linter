* Report and filter logic is generated as source and macro-executed, so TEXT ... ENDTEXT blocks build
* whole SELECT statements out of metadata rows. The body is output text, not code.
LPARAMETERS m.InfoType, m.Pid1

LOCAL m.SelectStatement, m.Rscript, m.CustVarCounter

m.CustVarCounter = 0

SELECT parentfile, parRelId AS pId, relation.fieldexpr2 AS fExpr ;
	FROM InfoFilt, relation ;
	WHERE InfoFilt.InfoType = m.InfoType ;
		.and. InfoFilt.parRelId = relation.mosnum ;
	INTO CURSOR ParentFilters NOFILTER

SCAN
	TEXT TO m.SelectStatement TEXTMERGE NOSHOW PRETEXT 1+2+4+8
		SELECT <<ParentFilters.pexpr2>>
			FROM <<ALLTRIM(ParentFilters.pfile2)>>
			WHERE <<ParentFilters.cexpr>> = <<ParentFilters.pexpr2>>
				AND <<ParentFilters.pex2>> = m.Pid1
			INTO CURSOR CheckParentFilters
	ENDTEXT
	&SelectStatement
	IF _TALLY = 0
		EXIT
	ENDIF
ENDSCAN

TEXT TO m.Rscript TEXTMERGE NOSHOW PRETEXT 1
	LOCAL m.rpt
	PRIVATE ALL LIKE DispExprVar*
	m.rpt = CreateTempReport("<<ALLTRIM(rptset.filename)>>")
	m.rpt.SetReportProperties("<<rptset.defaaction>>", <<IIF(rptset.AutoRefr, ".t.", ".f.")>>)
ENDTEXT

TEXT TO m.Rscript ADDITIVE TEXTMERGE NOSHOW PRETEXT 1
	m.rpt.AddColumn("<<ALLTRIM(rptcol.heading)>>", <<ALLTRIM(STR(rptcol.width))>>)
	m.rpt.Render()
ENDTEXT

* No TEXTMERGE: this one is a literal template with braces and quotes the parser must leave alone.
TEXT TO m.Rscript NOSHOW
	{ "unbalanced ( paren", 'IF this were code, ENDIF would be missing' }
ENDTEXT

RETURN m.Rscript
