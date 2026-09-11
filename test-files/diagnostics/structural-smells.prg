* Each structural smell once, with its clean counterpart alongside so the rule has to tell them apart.

* Unreachable: the RETURN leaves the routine before the next line can run.
PROCEDURE Unreachable
	LOCAL lnX
	lnX = 1
	RETURN lnX
	? "never runs"
ENDPROC

* Clean: a RETURN that is genuinely last, and one inside a branch.
PROCEDURE Fine
	IF .T.
		RETURN .F.
	ENDIF
	RETURN .T.
ENDPROC

* The second condition repeats the first, so its branch can never be reached.
PROCEDURE Duplicate
	LPARAMETERS tnType
	DO CASE
	CASE tnType = 1
		? "one"
	CASE tnType = 2
		? "two"
	CASE tnType = 1
		? "one again"
	ENDCASE
ENDPROC

* Branches that do nothing.
PROCEDURE Empty
	LOCAL llFlag
	llFlag = .T.
	IF llFlag
	ENDIF
	IF llFlag
		? "yes"
	ELSE
	ENDIF
	DO CASE
	CASE llFlag
	OTHERWISE
		? "no"
	ENDCASE
ENDPROC

* A TRY with neither CATCH nor FINALLY handles nothing.
PROCEDURE Swallow
	TRY
		? 1 / 0
	ENDTRY
ENDPROC

PROCEDURE Handled
	TRY
		? 1 / 0
	CATCH TO oErr
		? oErr.Message
	ENDTRY
ENDPROC

* PRIVATE ALL hides every variable of the caller; PRIVATE ALL LIKE is targeted and stays quiet.
PROCEDURE HideEverything
	PRIVATE ALL
	PRIVATE ALL LIKE l*
	? "done"
ENDPROC
