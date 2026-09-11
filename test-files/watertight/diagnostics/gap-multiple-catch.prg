* One TRY, two CATCH blocks: the first narrowed by WHEN to the error it can recover from, the second catching everything else. A single CATCH parses, and a single CATCH with a WHEN parses, but a second CATCH is not read at all -- the TRY is rejected and the file fails on the ENDTRY.
* This is the standard shape wherever the code retries: the lock-and-retry loops, the guarded USE that tolerates a missing .cdx, and the collection add that tolerates a duplicate key all write it.
LPARAMETERS m.cTable

LOCAL m.loErr, m.lFailed

m.lFailed = .f.

TRY
	USE (m.cTable) AGAIN IN 0
CATCH TO m.loErr WHEN m.loErr.ErrorNo = 1707
	m.lFailed = .t.
CATCH TO m.loErr
	m.lFailed = .t.
	=STRTOFILE(m.loErr.Message, "ErrorReports\lasterror.txt", .t.)
ENDTRY

RETURN !m.lFailed
