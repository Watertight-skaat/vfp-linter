* Bulk maintenance over a whole table: filter, walk, replace, and roll up. These run overnight, so
* the scoped REPLACE and the aggregate commands are preferred over a record-at-a-time loop.
LPARAMETERS m.cStnum, m.dAsOf, m.lDryRun

LOCAL m.nTouched, m.nBalance, m.nCount, m.cOldFilter, m.cOldOrder

IF !USED("invinfo")
	USE invinfo IN 0
ENDIF

SELECT invinfo
m.cOldFilter	= SET("FILTER")
m.cOldOrder		= ORDER()
m.nTouched		= 0

SET ORDER TO stact
SET FILTER TO stnum = m.cStnum .and. !inactive

CALCULATE CNT(), SUM(invbal), AVG(invbal), MAX(datedue) TO m.nCount, m.nBalance, m.nAverage, m.dLatest
SUM invbal FOR invbal > 0 TO m.nOwed

IF !m.lDryRun
	REPLACE ALL agedays WITH m.dAsOf - datedue FOR datedue < m.dAsOf .and. invbal # 0
	REPLACE ALL invcode WITH "D", stat WITH 2 ;
		FOR invbal > 0 .and. datedue < m.dAsOf - 90 ;
		IN invinfo
	m.nTouched = _TALLY
ENDIF

SCAN FOR invbal < 0
	IF ABS(invbal) < 0.005
		REPLACE invbal WITH 0
		LOOP
	ENDIF
	m.nTouched = m.nTouched + 1
ENDSCAN

DELETE ALL FOR EMPTY(mosnum) .and. invbal = 0
RECALL ALL FOR invnum = "PENDING"

SET FILTER TO .t.
IF !EMPTY(m.cOldOrder)
	SET ORDER TO (m.cOldOrder)
ENDIF
=FlushWrapper(.f., "invinfo")

RETURN m.nTouched
