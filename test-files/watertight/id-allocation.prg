* Allocating the next record id. There is no sequence and no unique constraint, so the counter table
* is locked, read, incremented, flushed and unlocked by hand -- the lock/increment/flush/unlock shape
* repeats for invoice numbers, batch numbers and every other human-facing sequence.
LPARAMETERS m.dbf, m.fld, m.idx, m.prefix, m.nBlockRequested

PRIVATE m.pathdbf, m.LargestCurrent, m.MosnumLength
LOCAL m.NextMos, m.CounterTable, m.WaitCleared, m.Padded

m.pathdbf		= ALLTRIM(UPPER(m.dbf))
m.CounterTable	= "MOSDBF"
m.Padded		= PADR(UPPER(JUSTSTEM(m.dbf)), 8)
m.prefix		= IIF(TYPE("m.prefix") = "C", m.prefix, "")
m.nBlockRequested = IIF(TYPE("m.nBlockRequested") = "N", MAX(m.nBlockRequested, 1), 1)

IF !SEEK(m.Padded + PADR(m.prefix, 4), m.CounterTable, "DBFprefix")
	INSERT INTO (m.CounterTable) (dbf, prefix) VALUES (m.Padded, m.prefix)
ENDIF

m.WaitCleared = .f.
DO WaitForRlock IN Procfile WITH m.CounterTable, "Acquiring new id # for " + ALLTRIM(files.filedesc2)

m.NextMos		= EVALUATE(m.CounterTable + ".char")
m.MosnumLength	= LEN(m.NextMos)

* VerifyUnique: the counter can fall behind if a table was restored from a backup, so the true
* maximum is recomputed rather than trusted.
SELECT MAX(&m.fld) AS largest FROM (m.pathdbf) INTO CURSOR curVerify NOFILTER
IF !EOF("curVerify") .and. curVerify.largest > m.NextMos
	m.NextMos = curVerify.largest
ENDIF
USE IN curVerify

REPLACE char WITH PADL(ALLTRIM(STR(VAL(m.NextMos) + m.nBlockRequested)), m.MosnumLength, "0") ;
	IN mosdbf
=FlushWrapper(.f., m.CounterTable)
UNLOCK IN mosdbf

RETURN PADL(ALLTRIM(STR(VAL(m.NextMos) + 1)), m.MosnumLength, "0")
