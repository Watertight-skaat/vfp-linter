* The `&name.` macro terminator followed immediately by a field reference, so `&cAlias..mosid` is the macro, its closing dot, and then `.mosid`. websync.PRG builds every one of its queries this way, because the alias is a chunk table whose name is only known at run time.
* A macro alias on its own parses. It is the doubled dot that fails, and it fails inside a SELECT list, so the query is rejected and its FROM and INTO lines are left standing as statements of their own.
LPARAMETERS m.cTableWebSyncAlias, m.cDataExpr

SELECT &cTableWebSyncAlias..mosid, &cTableWebSyncAlias..currlogmos AS clogmos ;
	FROM (m.cTableWebSyncAlias) ;
	INTO CURSOR WebSyncRecordSet READWRITE NOFILTER

RETURN .t.
