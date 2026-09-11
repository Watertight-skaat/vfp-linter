* SCAN accepts FOR and WHILE in either order and the app writes WHILE first, because the WHILE is what bounds the walk and the FOR is the extra filter. Reading them in a fixed order left `FOR ...` to the catch-all, which -- because it starts with FOR -- reported a missing ENDFOR for a block that was never opened: a false positive at error severity.
* run-scope-tests.js asserts that both operands reach the tree; this fixture asserts that neither is reported.
LPARAMETERS m.cKey

SELECT invinfo
SET ORDER TO stact
=SEEK(m.cKey)

SCAN REST WHILE stnum + acctnum == m.cKey FOR invbal # 0
	? invinfo.invnum
ENDSCAN
