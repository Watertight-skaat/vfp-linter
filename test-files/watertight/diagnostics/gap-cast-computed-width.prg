* `CAST(<expr> AS C(<expr>))` -- a SQL cast whose width is computed rather than written as a literal. The literal form is read; a width that is a variable is not, and the failure is not local to the cast: the whole SELECT becomes one unsupported statement, so its destination, its joins and its WHERE are all invisible to every rule.
* The widths come from the schema at runtime, which is why they are variables: the query pads to whatever the target column declares.
LPARAMETERS m.nFnameLen, m.nLnameLen

SELECT custinfo.stnum ;
		, custinfo.acctnum ;
		, CAST(custinfo.bfname AS C(m.nFnameLen)) AS fname ;
		, CAST(custinfo.blname AS C(m.nLnameLen)) AS lname ;
	FROM custinfo ;
	WHERE !EMPTY(custinfo.bemail) ;
	INTO CURSOR curNewContacts READWRITE
