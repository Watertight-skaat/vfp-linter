* FoxPro accepts any keyword abbreviated to its first four characters, and the 2.75-generation code uses that everywhere: ENDI for ENDIF, ENDD for ENDDO, ENDC for ENDCASE, DELE for DELETE, ACTI for ACTIVATE, EXCLU for EXCLUSIVE, DESC for DESCENDING.
* This was the most expensive gap in the tree, because an abbreviated block terminator was not recognised as a terminator at all: the block it was meant to close stayed open, every later ENDIF closed the wrong thing, and the file ended in a run of orphaned-terminator errors that named innocent lines. A dozen files in programs\legacy275 died this way on a single `endi`.
* The word is read whole now and measured against the keyword each site expects, so four characters are enough and `ENDIX` still closes nothing. Where two keywords share an abbreviation -- ENDD is ENDDO or ENDDEFINE -- the block that is open is what settles it.
PRIVATE m.sp_opt3, m.nCount

USE rbatcont EXCLU

IF m.sp_opt3
	@ PROW() + 0, 58 SAY "posted"
ENDI

m.nCount = 0
DO WHILE m.nCount < 10
	m.nCount = m.nCount + 1
ENDD

DELE FOR EMPTY(stnum)

RETURN .t.

DEFINE CLASS Batch AS Custom

	PROTECTED PROC Post
		RETURN .t.
	ENDP

	FUNC Total
		RETURN 0
	ENDF

ENDD
