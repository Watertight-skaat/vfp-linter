* A declaration list where an AS clause names a dotted type. `LOCAL loEx AS Exception` parses; `LOCAL loFSO AS Scripting.FileSystemObject` does not, and the rest of the line goes with it.
* This is the failure mode the ledger warns about, because it is silent where it matters: the statement reports itself, but the names after the dotted type are never recorded as declared, so every later write to them is reported as an implicit private. One unread declaration turns into a finding at every assignment -- 51 of them for a single `loEx` in foxbin2prg.prg -- and each one names a variable that is declared two lines up.
LOCAL lcTmpFile, loFSO AS Scripting.FileSystemObject, loEx AS Exception

lcTmpFile = "build.tmp"
loFSO = NULL
loEx = NULL

RETURN .t.
