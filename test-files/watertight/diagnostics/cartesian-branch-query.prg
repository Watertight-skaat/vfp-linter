* Multi-branch queries relate their tables on concatenated keys. When one of the joins is left out
* the query still runs, and on a real branch table it reads as a hang rather than an error.
LPARAMETERS m.cStnum

* stmast is never related to anything: every branch row is paired with every customer row.
SELECT custinfo.acctnum, custinfo.bname, stmast.stname ;
	FROM custinfo, stmast ;
	WHERE custinfo.stnum = m.cStnum ;
	INTO CURSOR curBranchCustomers

* Three tables, only two of them related.
SELECT invinfo.invnum, servaddr.addrnum, prodmast.part_no ;
	FROM invinfo, servaddr, prodmast ;
	WHERE invinfo.stnum + invinfo.acctnum = servaddr.stnum + servaddr.acctnum ;
	INTO CURSOR curOrphanProducts

* Related through a shared value rather than a direct comparison, which is fine.
SELECT invinfo.invnum, receipt.amount ;
	FROM invinfo, receipt ;
	WHERE invinfo.stnum = m.cStnum .and. receipt.stnum = m.cStnum ;
	INTO CURSOR curSameBranch

RETURN .t.
