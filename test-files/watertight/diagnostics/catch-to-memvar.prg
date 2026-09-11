* CATCH TO takes the m.-prefixed name house style puts on every variable. Reading only the bare identifier left `.ErrObj` over as its own statement -- and the clause read the wrong index of its own sequence besides, so `to` came back as the third character of the name.
* Now that the name arrives intact, CATCH TO is a write of it like any other, so an undeclared one is a real implicit PRIVATE that outlives the handler.
LPARAMETERS m.cTable

TRY
	USE (m.cTable) AGAIN IN 0
CATCH TO m.ErrObj
	=STRTOFILE(m.ErrObj.Message, "ErrorReports\lasterror.txt", .t.)
ENDTRY

RETURN .t.
