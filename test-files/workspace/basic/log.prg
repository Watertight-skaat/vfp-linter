#INCLUDE "shared.h"

PROCEDURE LogEntry
LPARAMETERS tcAccount
? m.tcAccount
ENDPROC

FUNCTION Describe(tcAccount, tnWidth)
RETURN PADR(m.tcAccount, m.tnWidth)
ENDFUNC
