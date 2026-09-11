* Stubs that were emptied out by putting a RETURN at the top and leaving the body below it. The old
* body still compiles, still reads like live code, and never runs -- this is the shape the
* unreachable-code rule exists for.
FUNCTION vNetLok
	LPARAMETERS m.NetOar, m.NetLoc, m.NetCom, m.NetMess, m.NetCode
	RETURN .t.

	LOCAL m.RetVar
	IF TYPE("_screen.MosDA.LockObject.DataSession") # "N"
		_screen.MosDa.AddProperty("LockObject", CREATEOBJECT("DataLocks"))
	ENDIF
	m.RetVar = _screen.MosDa.LockObject.TableLock(m.NetLoc, m.NetCom, m.NetMess, m.NetCode)
	RETURN m.RetVar
ENDFUNC


* The same thing inside a loop: the EXIT is unconditional, so the counter below it never moves.
PROCEDURE CountOpenItems
	LOCAL m.nCount
	m.nCount = 0
	SCAN
		IF invbal = 0
			LOOP
			m.nCount = m.nCount - 1
		ENDIF
		EXIT
		m.nCount = m.nCount + 1
	ENDSCAN
	RETURN m.nCount
ENDPROC
