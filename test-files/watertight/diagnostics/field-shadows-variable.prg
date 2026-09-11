* Variables named after columns. A bare reference resolves to the field while the table is open, so
* the assignment reaches the record instead of the variable -- which is why house style prefixes
* every memory variable with m.
PROCEDURE StampBranch
	LOCAL stnum, acctnum
	USE custinfo
	stnum	= "00001"
	acctnum	= "0001234"
	REPLACE stnum WITH m.stnum, acctnum WITH m.acctnum
ENDPROC


* The qualified reference is the other way the file shows that a name is a column.
PROCEDURE ReadBalance
	LOCAL invbal
	USE invinfo
	invbal = invinfo.invbal
	? m.invbal
ENDPROC


* Nothing here says these names are columns, so neither is reported.
PROCEDURE NoCollision
	LOCAL m.cBranch, m.nOwed
	USE custinfo
	m.cBranch	= "00001"
	m.nOwed		= 0
	? m.cBranch + TRANSFORM(m.nOwed)
ENDPROC
