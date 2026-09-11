* HAVING with no GROUP BY is a post-filter on the result set. It is a deliberate idiom in a few
* places here -- notably a self-join that drops the rows where both sides are the same -- so the rule
* reports it for information rather than as a mistake.
LPARAMETERS m.cStnum

SELECT U1.inv_curr AS fromCurrency, U2.inv_curr AS toCurrency ;
	FROM updateCurrencies U1, updateCurrencies U2 ;
	HAVING fromCurrency # toCurrency ;
	INTO CURSOR curConversions

* With the GROUP BY present it is a real aggregate filter and must stay quiet.
SELECT acctnum, SUM(invbal) AS owed ;
	FROM invinfo ;
	WHERE stnum = m.cStnum ;
	GROUP BY acctnum ;
	HAVING SUM(invbal) > 100 ;
	INTO CURSOR curBigDebtors

RETURN .t.
