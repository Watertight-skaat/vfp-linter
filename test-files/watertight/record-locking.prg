* The two locking layers: the application item lock that keeps two users out of the same business
* record, and the physical RLOCK/FLOCK underneath it with SET REPROCESS controlling the retry.
LPARAMETERS m.InfoKey, m.cTable, m.cMessage, m.cShareMode

LOCAL m.lOpened, m.nOldReprocess, m.lLocked

m.nOldReprocess = SET("REPROCESS")
m.lLocked		= .f.

m.lOpened = vOpenItem(m.InfoKey, m.cTable, m.cMessage, "E", m.cShareMode, .f., .null., .f., .f.)
IF !m.lOpened
	=MESSAGEBOX("Another user has this record open.", 48, "Watertight")
	RETURN .f.
ENDIF

SET REPROCESS TO 0
SELECT (m.cTable)
IF RLOCK()
	m.lLocked = .t.
	REPLACE inactive WITH .t.
	=FlushWrapper(.f., m.cTable)
	UNLOCK
ELSE
	DO WaitForRlock IN Procfile WITH m.cTable, "Waiting for " + ALLTRIM(m.cMessage)
	m.lLocked = .t.
	UNLOCK ALL
ENDIF

SET REPROCESS TO m.nOldReprocess
=vClosItem(m.InfoKey, m.cTable, "E", m.cShareMode, .null., m.cMessage)

RETURN m.lLocked


* The stubs the old call sites still reach. They return unconditionally; everything below the RETURN
* is kept only so the original body can be read.
FUNCTION NETLOK
	PARAMETERS NETOAR, NETLOC, NETCOM, NETMESS, NETCODE
	RETURN .T.
ENDFUNC

FUNCTION NETULOK
	PARAMETERS NETOAR, NETLOC, NETCOM, NETCODE
	RETURN .T.
ENDFUNC
