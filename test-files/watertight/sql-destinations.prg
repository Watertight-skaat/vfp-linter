* Everywhere a query result can be put. A query with no destination browses at run time, so every one
* of these names somewhere to land.
LPARAMETERS m.cTempTable, m.cTargetCursor

LOCAL m.cFieldList, m.cSource

m.cFieldList	= "custinfo.*"
m.cSource		= "custinfo"

SELECT * FROM custinfo WHERE !inactive INTO CURSOR curActive
SELECT * FROM custinfo WHERE !inactive INTO CURSOR curScratch READWRITE NOFILTER
SELECT * FROM custinfo WHERE !inactive INTO TABLE (m.cTempTable)
SELECT acctnum FROM custinfo WHERE !inactive INTO ARRAY laAccounts
SELECT acctnum, bname FROM custinfo WHERE !inactive TO FILE customers.txt
SELECT acctnum, bname FROM custinfo WHERE !inactive INTO CURSOR (m.cTargetCursor) NOFILTER

* The field list and the table can both arrive as macros, which is how the sync code drives one
* query over every table in the schema.
SELECT &cFieldList FROM (m.cSource) WHERE !inactive INTO CURSOR curMacroDriven READWRITE

INSERT INTO archive (stnum, acctnum, bname) ;
	SELECT stnum, acctnum, bname FROM custinfo WHERE inactive

RETURN .t.
