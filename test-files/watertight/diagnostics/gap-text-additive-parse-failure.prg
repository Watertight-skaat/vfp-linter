* TEXT ... ENDTEXT with ADDITIVE written anywhere other than immediately after the variable. The
* grammar requires TO <var> ADDITIVE adjacent, so the option list stops at NOSHOW, the TEXT rule
* fails, and the body is then read as code -- which makes the whole FILE fail to parse rather than
* producing a diagnostic. This is the one construct in the corpus that costs a file its diagnostics
* entirely, so it is kept on its own.
LOCAL m.Rscript

TEXT TO m.Rscript NOSHOW ADDITIVE TEXTMERGE PRETEXT 1
	m.rpt.AddColumn("<<ALLTRIM(rptcol.heading)>>")
ENDTEXT

RETURN m.Rscript
