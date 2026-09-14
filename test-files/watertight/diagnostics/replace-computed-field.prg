* REPLACE whose target field is computed rather than written out: a parenthesised expression, or a macro after an alias arrow. Both are how the metadata-driven code writes a column it only learns the name of at run time, and both are common enough that TranNum.prg and qbgetcustomfields.prg -- two files on the invoice path -- are unreadable without them.
* The failure was the expensive kind rather than the announced kind. REPLACE was rejected, so its WITH was left standing at the start of a statement, where WITH is the block that opens a member scope: the linter reported a missing ENDWITH and swallowed everything after it.
* Reading the target was only half of it. `IN (alias)` reads as the SQL IN operator to the expression rule, so the work area went into the value and the statement named no alias at all -- a clean parse and the wrong tree, which is what run-parse-tests.ts asserts against.
* `REPLACE ALL serial WITH "" FOR <condition>` is fine; it is only the computed target that fails.
LPARAMETERS m.cCursorName, m.cReturnType, m.nTranNumber

REPLACE ("I" + QBCustom.mosnum) ;
	WITH m.InfVal ;
	IN (m.cCursorName)

replace ;
	invcount->&cReturnType WITH MOD(m.nTranNumber, 99999) + 1 ;
	IN invcount

RETURN .t.
