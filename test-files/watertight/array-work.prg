* Arrays. SET COMPATIBLE OFF so DIMENSION keeps what is already there, PRIVATE ALL LIKE to keep a
* growing array visible to the routines called below, and the A* functions to search and sort it.
LPARAMETERS m.cStnum, m.cAcctnum

PRIVATE ALL LIKE tran_array*
LOCAL m.cnt, m.compatible_set, m.nFound, m.nRows, m.i

m.compatible_set = SET("compatible")
SET COMPATIBLE OFF

DIMENSION tran_array(1, 4)
LOCAL ARRAY laTotals(12), laGrid(4, 3)
m.cnt = 0

SELECT invnum, invbal, addrnum, staxcode ;
	FROM invinfo ;
	WHERE stnum + acctnum = m.cStnum + m.cAcctnum .and. invbal # 0 ;
	INTO CURSOR curAuto NOFILTER

SCAN
	m.cnt = m.cnt + 1
	DIMENSION tran_array(m.cnt, 4)
	tran_array(m.cnt, 1) = curAuto.invnum
	tran_array(m.cnt, 2) = curAuto.invbal
	tran_array(m.cnt, 3) = curAuto.addrnum
	tran_array(m.cnt, 4) = ALLTRIM(curAuto.staxcode)
ENDSCAN

m.nRows		= ALEN(tran_array, 1)
m.nFound	= ASCAN(tran_array, "0001234", 1, 0, 1, 8)
IF m.nFound > 0
	=ASORT(tran_array, 2, -1, 1)
ENDIF

FOR m.i = 1 TO 12
	laTotals[m.i] = 0
ENDFOR

FOR m.i = 1 TO m.nRows
	IF EMPTY(tran_array(m.i, 1))
		LOOP
	ENDIF
	laTotals[MONTH(DATE())] = laTotals[MONTH(DATE())] + tran_array(m.i, 2)
ENDFOR

IF m.compatible_set = "ON"
	SET COMPATIBLE ON
ENDIF

RETURN m.nRows
