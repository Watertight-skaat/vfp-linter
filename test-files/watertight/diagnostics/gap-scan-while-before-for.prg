* SCAN accepts FOR and WHILE in either order and the app writes WHILE first, because the WHILE is
* what bounds the walk and the FOR is the extra filter. The grammar only accepts FOR first, so the
* rest of the line falls through to the catch-all -- and because it starts with FOR, it is reported
* as an unterminated FOR block rather than as unsupported syntax.
LPARAMETERS m.cKey

SELECT invinfo
SET ORDER TO stact
=SEEK(m.cKey)

SCAN REST WHILE stnum + acctnum == m.cKey FOR invbal # 0
	? invinfo.invnum
ENDSCAN
