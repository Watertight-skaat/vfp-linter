* Index-tag lookups and record walks. Keys are fixed-width and padded, so ALLTRIM on read and PADR on
* seek are everywhere; SET ORDER TO picks the compound tag before the SEEK.
LPARAMETERS m.cStnum, m.cAcctnum, m.dCutoff

LOCAL m.Balance, m.Count, m.Key

m.Key		= PADR(m.cStnum, 5) + PADR(m.cAcctnum, 7)
m.Balance	= 0
m.Count		= 0

IF !USED("invinfo")
	USE invinfo IN 0
ENDIF

SELECT invinfo
SET ORDER TO stact
IF !SEEK(m.Key)
	RETURN 0
ENDIF

SCAN REST FOR invbal # 0 WHILE PADR(stnum, 5) + PADR(acctnum, 7) == m.Key
	IF datedue > m.dCutoff
		LOOP
	ENDIF
	m.Balance	= m.Balance + invbal
	m.Count		= m.Count + 1
ENDSCAN

* The same walk without the index, for the cases where no tag covers the filter.
SELECT invinfo
GO TOP
LOCATE FOR stnum + acctnum == m.Key .and. invcode = "P"
DO WHILE FOUND()
	m.Count = m.Count + 1
	SKIP
	IF EOF()
		EXIT
	ENDIF
ENDDO

SET ORDER TO
GO BOTTOM
? m.Balance, m.Count

RETURN m.Balance
