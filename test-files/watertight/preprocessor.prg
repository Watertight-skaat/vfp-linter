* Compile-time switches. Constants come from a shared header, feature work is fenced off with #IF so
* a half-finished path can sit in the tree without running.
#INCLUDE S:\Libs\Watertight.WebClients.h
#DEFINE CRLF			CHR(13) + CHR(10)
#DEFINE TAB				CHR(9)
#DEFINE MOSNUM_WIDTH	8
#DEFINE STNUM_WIDTH		5
#DEFINE MSG_SEND_NOW	1
#DEFINE HRESULT_FAILURE	0x80000000

LPARAMETERS m.cStnum, m.cAcctnum

LOCAL m.cKey, m.cReport

m.cKey		= PADR(m.cStnum, STNUM_WIDTH) + PADR(m.cAcctnum, 7)
m.cReport	= "Account " + ALLTRIM(m.cAcctnum) + CRLF + REPLICATE(TAB, 2) + "Key: " + m.cKey + CRLF

#IF TYPE("m.dev") = "L" AND m.dev
	* Development builds talk to the staging relay and log every payload.
	m.cReport = m.cReport + "[dev build]" + CRLF
	=STRTOFILE(m.cReport, "ErrorReports\devtrace.txt", .t.)
#ENDIF

#IF .f.
	* The rewritten posting path, not turned on yet.
	DO PostChargesV2 WITH m.cStnum, m.cAcctnum
#ELSE
	DO PostCharges WITH m.cStnum, m.cAcctnum
#ENDIF

RETURN m.cReport
