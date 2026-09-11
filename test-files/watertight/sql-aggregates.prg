* Set and aggregate work. Single records are fetched with SEEK; SQL is reserved for rollups, and the
* results always land in a cursor rather than a Browse window.
LPARAMETERS m.cStnum, m.dFrom, m.dThru

LOCAL m.nAging

SELECT ;
	  stnum ;
	, acctnum ;
	, SUM(invbal)								AS balance ;
	, SUM(IIF(datedue < m.dFrom, invbal, 0))	AS aged ;
	, COUNT(*)									AS invoices ;
	, MAX(datedue)								AS lastdue ;
	FROM invinfo ;
	WHERE stnum = m.cStnum .and. invbal # 0 ;
	GROUP BY 1, 2 ;
	HAVING SUM(invbal) > 0 ;
	ORDER BY 3 DESC ;
	INTO CURSOR curAging NOFILTER

* Clause order is not fixed in this dialect: GROUP BY is routinely written before WHERE.
SELECT stnum, ratecode AS code, MIN(rate) AS minrate, MAX(rate) AS maxrate ;
	FROM charcont ;
	GROUP BY stnum, ratecode ;
	WHERE !EMPTY(ratecode) .and. rate # 0 ;
	INTO CURSOR curUnitRate ;
	NOFILTER

* A UNION carries the destination on its last leg.
SELECT stnum, acctnum, amount, "CHG" AS source FROM charcred WHERE datechgd BETWEEN(m.dFrom, m.dThru) ;
	UNION ALL ;
	SELECT stnum, acctnum, -amount AS amount, "PAY" AS source FROM receipt WHERE date BETWEEN(m.dFrom, m.dThru) ;
	INTO CURSOR curLedger READWRITE

* A derived table, which is how a two-level rollup is done without a temp file.
SELECT MIN(firstchg) AS firstchg, MAX(lastchg) AS lastchg ;
	FROM ( ;
		SELECT MIN(datechgd) AS firstchg, MAX(datechgd) AS lastchg ;
			FROM charcred ;
			WHERE datechgd # {} ;
		UNION ;
		SELECT MIN(date) AS firstchg, MAX(date) AS lastchg ;
			FROM receipt ;
			WHERE date # {}) spans ;
	INTO CURSOR curSpan

SELECT TOP 25 acctnum, SUM(invbal) AS owed ;
	FROM invinfo ;
	GROUP BY 1 ;
	ORDER BY 2 DESC ;
	INTO ARRAY laWorst

m.nAging = IIF(_TALLY = 0, 0, curAging.balance)

RETURN m.nAging
