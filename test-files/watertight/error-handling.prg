* The global ON ERROR handler and the local TRY/CATCH the app uses for recoverable file work. The
* handler is a long DO CASE on the VFP error number that decides between ignore, retry and report.
LPARAMETERS m.errNum, m.errDesc, m.errLine, m.errProg, m.errProc, m.errCode

LOCAL m.lIgnorable, m.Result, m.oErr

m.lIgnorable	= .f.
m.Result		= 0

IF PCOUNT() = 0	&& called only to install the handler
	ON ERROR DO MosesErrorHandler WITH ERROR(), MESSAGE(), LINENO(), PROGRAM(), SYS(16), 0
	RETURN .t.
ENDIF

DO CASE
	CASE m.errNum = 30		&& Row or column position is off the screen.
		m.lIgnorable = .t.
	CASE m.errNum = 39		&& Numeric overflow. Data was lost.
		m.lIgnorable = .t.
	CASE m.errNum = 125		&& Printer is not ready.
		m.lIgnorable = .t.
	CASE m.errNum = 2066 .and. TYPE("_screen.last_indexCorrupt") # "N"
		* First sighting of a corrupt index: back off a random interval and retry.
		_screen.AddProperty("last_indexCorrupt", SECONDS())
		DO RandomDelay WITH .5, 1.5
		m.Result = 4
	CASE (m.errNum = 108 .or. m.errNum = 109) .and. SECONDS() - _screen.last_fileinuse < 15
		* File or record in use, and it has not been long enough to give up.
		DO RandomDelay WITH .5, 1
		m.Result = 4
	OTHERWISE
		m.Result = 1
ENDCASE

IF m.lIgnorable
	RETURN .t.
ENDIF

* Local recovery: a guarded USE whose only failure mode is "the file is not there yet".
TRY
	IF !USED("kdserror")
		USE (m.m_tpath + "kdserror") AGAIN IN 0
	ENDIF
CATCH
ENDTRY

TRY
	INSERT INTO kdserror (errnum, errdesc, errprog, errline, whenlogged) ;
		VALUES (m.errNum, LEFT(m.errDesc, 240), m.errProg, m.errLine, DATETIME())
	=FlushWrapper(.f., "kdserror")
CATCH TO oErr
	=STRTOFILE(oErr.Message + CHR(13) + CHR(10), "ErrorReports\lasterror.txt", .t.)
	THROW
FINALLY
	IF USED("kdserror")
		USE IN kdserror
	ENDIF
ENDTRY

RETURN m.Result
