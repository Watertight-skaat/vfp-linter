* The restructure and repair utilities: build a new structure, copy the data across, rebuild the tags
* and swap the files. These are the only places PACK and ZAP appear.
LPARAMETERS m.cTable, m.cBeforeFile, m.lRebuildOnly

LOCAL m.cTemp, m.nRecords, m.cKeyExpr, m.cTagName

m.cTemp		= "restruct_temp"
m.cKeyExpr	= "stnum+acctnum"
m.cTagName	= "stact"

CREATE CURSOR cdxFileList (fname c(50), fsize n(10), fdate d(8), ftime c(12))
CREATE CURSOR TableList (TableName c(8), TableNum n(3), progress n(1))

CREATE TABLE (m.cTemp) FREE ;
	(mosnum		c(8) NOT NULL, ;
	 stnum		c(5), ;
	 acctnum	c(7), ;
	 bname		c(40), ;
	 invbal		n(12,2) DEFAULT 0, ;
	 datechgd	d(8), ;
	 inactive	l(1), ;
	 comments	m(4))

SELECT 0
USE (m.cTable) EXCLUSIVE
m.nRecords = RECCOUNT()

IF !m.lRebuildOnly
	SELECT 0
	USE (m.cTemp) EXCLUSIVE
	APPEND FROM (m.cBeforeFile)
	APPEND FROM (DBF("curStaging")) FOR !inactive
	COPY TO (m.cTable + "_backup") WITH CDX
	COPY TO (m.cTable + "_export") FIELDS mosnum, stnum, acctnum TYPE CSV
ENDIF

SELECT (m.cTable)
DELETE ALL FOR EMPTY(mosnum)
PACK
INDEX ON &cKeyExpr TAG &cTagName
INDEX ON mosnum TAG mosnum CANDIDATE
INDEX ON UPPER(bname) TAG bname FOR !inactive ADDITIVE

IF m.nRecords = 0
	ZAP
ENDIF

USE
CLOSE TABLES
ERASE (m.cTemp + ".dbf")
ERASE (m.cTemp + ".cdx")
ERASE (m.cTemp + ".fpt")

RETURN m.nRecords
