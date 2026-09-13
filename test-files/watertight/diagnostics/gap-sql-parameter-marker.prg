* The `?` parameter marker, which names a memory variable in a query without macro substitution. The QuickBooks integration uses it throughout.
* The cost is not the unread statement but the finding it invents: the WHERE is rejected, so the INTO CURSOR that follows it is orphaned, and select-without-into then reports a query that has a destination written two lines below it. A gap that only announced itself would be cheaper than one that makes a rule lie.
LPARAMETERS m.lcAcctnum

SELECT * FROM custinfo ;
	WHERE acctnum = ?lcAcctnum ;
	INTO CURSOR Found

RETURN .t.
