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
SET ORDER TO TAG stnum OF custinfo DESCENDING IN (m.lcAlias)
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

* The pre-SQL data commands. Each names a table, a field or a variable, so each carries operands a rule would want.
TOTAL ON stnum TO summary FIELDS invbal FOR invbal > 0
JOIN WITH (m.lcAlias) TO joined FOR custinfo.stnum = invinfo.stnum FIELDS custinfo.stnum, invinfo.invbal
UPDATE ON stnum FROM (m.lcAlias) REPLACE invbal WITH invinfo.invbal, invnum WITH invinfo.invnum RANDOM
COPY STRUCTURE TO newtbl FIELDS stnum, invbal WITH CDX
COPY STRUCTURE EXTENDED TO structtbl
DELETE TAG stnum, acctnum
DELETE TAG ALL OF custinfo
BLANK FIELDS invbal NEXT 1 IN (m.lcAlias)

* Memory-variable files, the remaining PRIVATE form, and the debugging commands.
SAVE TO config.mem ALL LIKE m_*
SAVE TO MEMO notes
RESTORE FROM config.mem ADDITIVE
RESTORE FROM MEMO notes
ASSERT m.lnRows > 0 MESSAGE "no rows"
ASSERT (m.lnRows > 0)
PLAY MACRO F5 TIMES 3

PROCEDURE Hidden
	PRIVATE ALL EXCEPT m_*
	? "hidden"
ENDPROC

PROCEDURE OldStyleParams
	PARAM MNM, ARR, FLD
	? m.MNM + m.ARR + m.FLD
ENDPROC

PROCEDURE SingularParam
	PARAMETER cName
	? m.cName
ENDPROC
