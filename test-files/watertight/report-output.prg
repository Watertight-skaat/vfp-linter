* Printing. There is no report writer here: layouts are rows in a table and every line is emitted by
* a dispatcher that decides between plain text, the laser stack and a spreadsheet.
PARAMETERS ;
	  m.dispval ;
	, m.prow, m.pcol ;
	, m.xrow, m.xcol ;
	, m.lrow, m.lcol ;
	, m.lfheader, m.lpheader ;
	, m.lnewitem, m.lnewform ;
	, m.x_numberformat, m.xfillrgb, m.xtextrgb ;
	, m.nWrapRowHeight

LOCAL m.lfontalign, m.lwidth, m.lheight, m.cPHeadFilt, m.cPFootFilt

m.cPHeadFilt	= IIF(TYPE("m.lpheader") = "C", m.lpheader, "")
m.cPFootFilt	= IIF(TYPE("m.lpfooter") = "C", m.lpfooter, "")
m.lfontalign	= ""
m.lwidth		= 0
m.lheight		= 0

DO CASE
	CASE VARTYPE(m.print_job_output) # "C" .or. "text" $ m.print_job_output
		IF VARTYPE(m.prow) = "N"
			IF SET("device") = "SCREEN" .or. m.prow > 2000
				m.prow = m.prow - PROW()
				=prntcolor("PRINTER")
			ENDIF
			@ m.prow, m.pcol SAY m.dispval
			@ PROW()+1, 1
		ENDIF

	CASE "laser" $ m.print_job_output
		IF VARTYPE(m.lcol) = "C" .and. OCCURS(",", m.lcol) > 1
			m.lfontalign	= SUBSTR(m.lcol, AT(",", m.lcol, 2) + 1, 1)
			m.lwidth		= VAL(SUBSTR(m.lcol, AT(",", m.lcol, 1) + 1))
		ENDIF
		=LaserText(m.dispval, m.lrow, m.lcol, m.lwidth, m.lfontalign)

	CASE "excel" $ m.print_job_output
		=XlsCell(m.xrow, m.xcol, m.dispval, m.x_numberformat, m.xfillrgb, m.xtextrgb)

	OTHERWISE
		? m.dispval
ENDCASE

RETURN .t.


PROCEDURE DrawFrame
	PARAMETERS m.nWidth, m.nHigh, m.nCol, m.nF1, m.nF2, m.nF3

	@ 0.480, m.nCol					TO 3.654, m.nCol + m.nWidth		PATTERN 1 PEN 1, 8 STYLE "16" COLOR RGB(,,,m.nF1,m.nF2,m.nF3)
	@ 0.576, m.nCol + 0.200			TO 2.499, m.nCol + (m.nWidth - 0.200) PEN 1, 8 STYLE "16" COLOR RGB(255,255,255,,,)
	@ 2.115, 1						TO m.nHigh + 0.780, 3 + m.nWidth PATTERN 1 PEN 1, 8 COLOR RGB(,,,m.nF1,m.nF2,m.nF3)
	@ 0.730, m.nCol					SAY "Statement" PICTURE "@I" SIZE 0.938, m.nWidth FONT "MS Sans Serif", 8 STYLE "T"
	@ 2.500, 2						TO WROWS() - 0.5, WCOLS() - 2 PATTERN 1 COLOR RGB(,,,m.nF1,m.nF2,m.nF3) PEN 1,0
	@ 5, 10 CLEAR
	@ PROW() + 1, 0

	RETURN
ENDPROC
