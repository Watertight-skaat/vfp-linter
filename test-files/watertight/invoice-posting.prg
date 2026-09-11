* Posting an invoice. There are no transactions, so the writes are ordered so that a failure halfway
* leaves the data recoverable, and each table is flushed as soon as it is written.
LPARAMETERS m.cStnum, m.cAcctnum, m.nAddrnum, m.dPostDate, m.cBatch

LOCAL m.cInvNum, m.cInvoiceId, m.nTotal, m.nTax, m.lPosted, m.cTaxCode

m.lPosted	= .f.
m.nTotal	= 0
m.nTax		= 0

IF !SEEK(PADR(m.cStnum, 5) + PADR(m.cAcctnum, 7), "custinfo", "stact")
	=MESSAGEBOX("Unknown account " + ALLTRIM(m.cAcctnum), 16, "Post Invoice")
	RETURN .f.
ENDIF

m.cTaxCode = IIF(SEEK(PADR(m.cStnum, 5) + PADR(m.cAcctnum, 7) + STR(m.nAddrnum), "servaddr", "st_ac_ad"), ;
					ALLTRIM(servaddr.staxcode), "")

SELECT SUM(amount) AS charged, SUM(taxamt) AS taxed ;
	FROM trancur ;
	WHERE !posted ;
	INTO CURSOR curTotals
m.nTotal	= NVL(curTotals.charged, 0)
m.nTax		= NVL(curTotals.taxed, 0)
USE IN curTotals

IF m.nTotal = 0 .and. m.nTax = 0
	RETURN .f.
ENDIF

m.cInvNum		= trannum("invcount", m.cStnum)
m.cInvoiceId	= m_mnum("invinfo")

INSERT INTO invinfo (mosnum, stnum, acctnum, addrnum, invnum, invdate, invtot, invbal, invcode, staxcode, batchnum) ;
	VALUES (m.cInvoiceId, PADR(m.cStnum, 5), PADR(m.cAcctnum, 7), m.nAddrnum, m.cInvNum, m.dPostDate, ;
			m.nTotal + m.nTax, m.nTotal + m.nTax, "I", PADR(m.cTaxCode, 6), m.cBatch)
=FlushWrapper(.f., "invinfo")

SELECT trancur
SCAN FOR !posted
	INSERT INTO charcred (mosnum, stnum, acctnum, addrnum, invnum, datechgd, amount, code, prodnum) ;
		VALUES (m_mnum("charcred"), PADR(m.cStnum, 5), PADR(m.cAcctnum, 7), m.nAddrnum, m.cInvNum, ;
				m.dPostDate, trancur.amount, trancur.code, trancur.prodnum)
	REPLACE posted WITH .t., invnum WITH m.cInvNum IN trancur
ENDSCAN
=FlushWrapper(.f., "charcred")

REPLACE lastinv WITH m.dPostDate, ;
		balance WITH custinfo.balance + m.nTotal + m.nTax ;
	IN custinfo
=FlushWrapper(.f., "custinfo")

=DataLogCheck("invinfo", m.cInvoiceId)
m.lPosted = .t.

RETURN m.lPosted
