* TEXT ... ENDTEXT with ADDITIVE written somewhere other than immediately after the variable. The option list is order-free, so every spelling of it parses; requiring TO <var> ADDITIVE adjacent stopped the list at NOSHOW, failed the TEXT rule, and left the body to be read as code -- which cost the whole FILE its parse rather than producing one diagnostic.
* Kept as its own fixture because it is the only construct the corpus found that took a file's diagnostics away entirely. A `.expected` file appearing beside it means that is happening again.
LOCAL m.Rscript

TEXT TO m.Rscript NOSHOW ADDITIVE TEXTMERGE PRETEXT 1
	m.rpt.AddColumn("<<ALLTRIM(rptcol.heading)>>")
ENDTEXT

RETURN m.Rscript
