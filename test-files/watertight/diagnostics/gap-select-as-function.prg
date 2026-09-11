* SELECT() the function -- the work area a name resolves to, or the current one when called bare. SELECT is also the command that switches work areas, and the command wins, so the call is not read as an expression at all: the condition around it fails, the IF is rejected, and the file dies on the orphaned ENDIF. Saving and restoring the current work area around a lookup is the single most repeated idiom in this source, which makes this the same gap as PARAMETERS() and about as expensive.
* `STORE SELECT(0) TO m.nArea` does parse, because STORE reaches the expression by a different path. That is what makes the gap hard to notice by reading: the same call is read in one statement and not in the next.
LOCAL m.nArea, m.lReady

m.nArea = SELECT()

IF SELECT("tickler") > 0 .and. FILE("TICKLER.DBF")
	SELECT tickler
	LOCATE FOR .t.
	m.lReady = FOUND()
	USE
ENDIF

SELECT (m.nArea)

RETURN m.lReady
