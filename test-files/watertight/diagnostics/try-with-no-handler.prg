* A TRY with neither CATCH nor FINALLY handles nothing: the error still reaches the global ON ERROR
* handler, so the block only makes it look guarded.
LPARAMETERS m.cTable

TRY
	IF !USED(m.cTable)
		USE (m.cTable) AGAIN IN 0
	ENDIF
ENDTRY

* An intentionally empty CATCH is a different thing -- it really does swallow the error -- so it is
* not reported.
TRY
	USE (m.cTable + "_archive") AGAIN IN 0
CATCH
ENDTRY

RETURN .t.
