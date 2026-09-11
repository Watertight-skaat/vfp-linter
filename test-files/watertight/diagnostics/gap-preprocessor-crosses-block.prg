* A #IF fence that does not nest with the block structure around it: the IF opens outside the fence and its ENDIF sits inside. VFP's preprocessor is a text pass that runs before the compiler, so the ENDIF is simply there or not there depending on the constant; the grammar treats #IF as a block of its own, so the ENDIF lands inside the wrong one.
* This does not fail the parse -- it reports `unterminated-block` at error severity against an IF that is terminated, which is worse in one way: the file looks checked, and the error names a problem the code does not have.
#DEFINE RECENT_TRANS 1

LPARAMETERS m.nRow

IF m.nRow > 0
	DO print_set WITH "Arial", 8, "BOLD", .f., "L"
#IF RECENT_TRANS
	DO print_line WITH "Recent Transactions:", .f., .f., m.nRow, 10
ENDIF
#ENDIF

RETURN .t.
