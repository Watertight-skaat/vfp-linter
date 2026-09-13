* FoxPro accepts any keyword abbreviated to its first four characters, and the 2.75-generation code uses that everywhere: ENDI for ENDIF, ENDD for ENDDO, ENDC for ENDCASE, DELE for DELETE, ACTI for ACTIVATE, EXCLU for EXCLUSIVE, DESC for DESCENDING.
* This is the most expensive gap in the tree, because an abbreviated block terminator is not recognised as a terminator at all: the block it was meant to close stays open, every later ENDIF closes the wrong thing, and the file ends in a run of orphaned-terminator errors that name innocent lines. A dozen files in programs\legacy275 die this way on a single `endi`.
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
