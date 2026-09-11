* The Xbase housekeeping and record commands, each here to prove it still parses. This is a top-level fixture, so an `.expected` file appearing beside it means one of them regressed into the catch-all and is no longer being checked by any rule.
LOCAL lnRows, lnAvg, lnOverdue, loRecord, lcAlias, lcPath, lcReport
LOCAL ARRAY laRow(2)
lcAlias	= "invinfo"
lcPath	= "custinfo"

* A record becomes an object, and an object becomes a record.
SCATTER MEMVAR
SCATTER NAME m.loRecord MEMO
SCATTER MEMO NAME m.loRecord
SCATTER FIELDS invnum, invbal TO laRow
GATHER NAME m.loRecord MEMO
GATHER MEMVAR

* One command with three names and one option tail.
COUNT TO lnRows
COUNT FOR invbal > 0 TO lnOverdue
AVERAGE invbal TO lnAvg
SUM invbal TO lnRows

FLUSH
FLUSH FORCE
REINDEX
MD datalog
RD datalog
CONTINUE
NODEFAULT
PUSH KEY
POP KEY
EXTERNAL ARRAY laExternal
MODIFY STRUCTURE
ALTER TABLE custinfo ADD COLUMN websync l(1)
RUN /N notepad.exe

* The flags sit after the message at every call site, and a bare TO clears the setting.
WAIT WINDOW "Resizing graphs..." NOWAIT NOCLEAR
WAIT WINDOW NOWAIT "Rebuilding"
WAIT WINDOW "Waiting" TIMEOUT 3
SET FILTER TO

* A work area named by an expression, wherever an alias is expected.
USE IN (m.lcAlias)
USE (m.lcPath) AGAIN ALIAS (m.lcAlias) IN 0
SET RELATION TO stnum + acctnum INTO (m.lcAlias)
SET ORDER TO (m.lcAlias) DESCENDING IN (m.lcAlias)
GO TOP IN (m.lcAlias)

* ADDITIVE anywhere in the option list, not only next to the variable.
TEXT TO m.lcReport NOSHOW ADDITIVE TEXTMERGE PRETEXT 1
	<<ALLTRIM(rptcol.heading)>>
ENDTEXT

* WHILE before FOR, which is the order the walk reads in.
SCAN REST WHILE stnum + acctnum == m.lcAlias FOR invbal # 0
	? invinfo.invnum
ENDSCAN

? m.lnRows + m.lnAvg + m.lnOverdue
? m.lcReport
? laRow(1)

PROCEDURE OldStyleParams
	PARAM MNM, ARR, FLD
	? m.MNM + m.ARR + m.FLD
ENDPROC

PROCEDURE SingularParam
	PARAMETER cName
	? m.cName
ENDPROC
