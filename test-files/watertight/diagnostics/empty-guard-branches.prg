* Guard clauses that were emptied out, leaving only the comment that explained them. Comments are not
* part of the tree, so a branch holding nothing else reads as empty -- advisory rather than a warning
* for exactly that reason.
LPARAMETERS m.cStnum, m.oRecord

IF EMPTY(m.cStnum)
	* handled upstream now
ELSE
	m.cStnum = PADR(m.cStnum, 5)
ENDIF

IF m.oRecord.inactive
	RETURN .f.
ELSE
ENDIF

DO CASE
	CASE m.oRecord.invbal > 0
		=PostDebit(m.oRecord)
	CASE m.oRecord.invbal < 0
		* credits are posted by the nightly job
	OTHERWISE
ENDCASE

RETURN .t.
