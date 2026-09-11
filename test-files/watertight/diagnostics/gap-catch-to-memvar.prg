* CATCH TO takes the m.-prefixed name that house style puts on every variable. The grammar reads
* only the bare identifier, so the .ErrObj half of the name is left over as its own statement.
LPARAMETERS m.cTable

TRY
	USE (m.cTable) AGAIN IN 0
CATCH TO m.ErrObj
	=STRTOFILE(m.ErrObj.Message, "ErrorReports\lasterror.txt", .t.)
ENDTRY

RETURN .t.
