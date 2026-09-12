* The transaction frame: a BEGIN that is never closed, and an exit that steps over the close, each with the clean counterpart alongside.

* File-level code has a frame of its own, and this one is closed.
BEGIN TRANSACTION
REPLACE settings.nRunCount WITH settings.nRunCount + 1
END TRANSACTION

* Never closed: the routine ends with the writes uncommitted and the locks still held. The END TRANSACTION in the routines below is not this one's.
PROCEDURE PostOrphan
	BEGIN TRANSACTION
	REPLACE orders.cStatus WITH "P"
ENDPROC

* The close is there, but the early RETURN leaves before reaching it.
PROCEDURE PostEarlyReturn
	BEGIN TRANSACTION
	REPLACE orders.cStatus WITH "P"
	IF NOT INLIST(orders.cStatus, "P")
		RETURN .F.
	ENDIF
	END TRANSACTION
	RETURN .T.
ENDPROC

* Clean: the early exit unwinds the frame before it goes.
PROCEDURE PostRolledBack
	BEGIN TRANSACTION
	REPLACE orders.cStatus WITH "P"
	IF NOT INLIST(orders.cStatus, "P")
		ROLLBACK
		RETURN .F.
	ENDIF
	END TRANSACTION
	RETURN .T.
ENDPROC

* Clean: the frame is unwound in the CATCH, which sits after the close rather than before it.
PROCEDURE PostInTry
	LOCAL loErr
	TRY
		BEGIN TRANSACTION
		REPLACE orders.cStatus WITH "P"
		END TRANSACTION
	CATCH TO loErr
		ROLLBACK
		? loErr.Message
	ENDTRY
ENDPROC

* One arm commits and the other leaves without closing, so the close cannot be claimed for the path that returns.
PROCEDURE PostBranchCommit
	BEGIN TRANSACTION
	REPLACE orders.cStatus WITH "P"
	IF INLIST(orders.cStatus, "P")
		END TRANSACTION
	ELSE
		RETURN .F.
	ENDIF
	RETURN .T.
ENDPROC

* Clean: TXNLEVEL() guards the unwind, because ROLLBACK with nothing open is itself an error. The close is conditional, so nothing after it is claimed either way.
PROCEDURE PostGuardedRollback
	BEGIN TRANSACTION
	REPLACE orders.cStatus WITH "P"
	IF NOT INLIST(orders.cStatus, "P")
		IF TXNLEVEL() > 0
			ROLLBACK
		ENDIF
		RETURN .F.
	ENDIF
	END TRANSACTION
	RETURN .T.
ENDPROC

* Clean: the close is in the FINALLY, which every path runs.
PROCEDURE PostInFinally
	BEGIN TRANSACTION
	TRY
		REPLACE orders.cStatus WITH "P"
	FINALLY
		END TRANSACTION
	ENDTRY
ENDPROC

* A RETURN inside a loop leaves the frame open just as one at the top level does.
PROCEDURE PostInScan
	BEGIN TRANSACTION
	SCAN
		REPLACE orders.cStatus WITH "P"
		IF NOT INLIST(orders.cStatus, "P")
			RETURN .F.
		ENDIF
	ENDSCAN
	END TRANSACTION
ENDPROC
