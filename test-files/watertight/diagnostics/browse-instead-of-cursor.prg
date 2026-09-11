* A query with nowhere to put its result opens a Browse window at run time. Inside a posting routine
* that is an unfinished query, not an intention -- usually the INTO clause was lost in an edit.
LPARAMETERS m.cStnum, m.cAcctnum

SELECT invnum, invbal ;
	FROM invinfo ;
	WHERE stnum + acctnum = m.cStnum + m.cAcctnum

SELECT SUM(amount) AS charged FROM charcred WHERE stnum = m.cStnum

* These two are fine: a subquery has no destination of its own, and the UNION carries the INTO on
* its last leg.
SELECT acctnum FROM custinfo ;
	WHERE stnum = m.cStnum ;
		.and. acctnum IN (SELECT acctnum FROM invinfo WHERE invbal > 0) ;
	INTO CURSOR curOwing

SELECT acctnum, "OPEN" AS state FROM invinfo WHERE invbal > 0 ;
	UNION ALL ;
	SELECT acctnum, "PAID" AS state FROM invinfo WHERE invbal = 0 ;
	INTO CURSOR curStates

RETURN .t.
