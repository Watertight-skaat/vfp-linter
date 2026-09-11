* Joins across the branch/customer/address key chain. The old tables have no declared relationships,
* so parents and children are matched on concatenated fixed-width keys.
LPARAMETERS m.cStnum, m.cAcctnum

LOCAL m.nRows

SELECT invinfo.invnum, invinfo.invbal, servaddr.addrnum, servaddr.staxcode ;
	FROM invinfo, servaddr ;
	WHERE invinfo.stnum + invinfo.acctnum + STR(invinfo.addrnum) ;
			= servaddr.stnum + servaddr.acctnum + STR(servaddr.addrnum) ;
		.and. invinfo.stnum + invinfo.acctnum = m.cStnum + m.cAcctnum ;
		.and. invinfo.invbal # 0 ;
	INTO CURSOR curOpenItems NOFILTER

SELECT c.acctnum, c.bname, NVL(r.lodesc, SPACE(20)) AS route ;
	FROM custinfo c ;
		LEFT OUTER JOIN routedrv r ON c.stnum + c.routecode = r.stnum + r.routecode ;
	WHERE c.stnum = m.cStnum .and. !c.inactive ;
	INTO CURSOR curRoutes

* An INNER JOIN and a comma-joined table in the same FROM, which the old queries mix freely.
SELECT p.part_no, p.prod_desc, s.swapcat, l.lodesc ;
	FROM prodmast p ;
		INNER JOIN partswap s ON p.stnum + p.part_no = s.stnum + s.part_no ;
		, location l ;
	WHERE l.stnum = p.stnum .and. p.acti = 1 ;
	INTO CURSOR curSwappable READWRITE

* A correlated subquery in the WHERE, and an IN list against another query.
SELECT acctnum, bname ;
	FROM custinfo ;
	WHERE stnum = m.cStnum ;
		.and. EXISTS(SELECT 1 FROM invinfo WHERE invinfo.stnum + invinfo.acctnum = custinfo.stnum + custinfo.acctnum .and. invinfo.invbal > 0) ;
		.and. billperiod IN ("M", "Q", "A") ;
	INTO CURSOR curDelinquent

SELECT COUNT(*) AS rows FROM curDelinquent INTO ARRAY laCount
m.nRows = laCount[1]

RETURN m.nRows
