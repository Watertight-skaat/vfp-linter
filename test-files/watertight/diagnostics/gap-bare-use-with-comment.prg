* A bare USE -- the form that closes the current work area -- with a trailing `&&` comment. USE alone parses, and USE with an alias parses, but the comment after the bare form is read as the file name: the words in it become statements, and any `if` among them opens a block that never closes.
* The comment above the close is near-universal in this code, so this turns an ordinary FINALLY into an unterminated IF.
LOCAL m.nPreviousArea
m.nPreviousArea = SELECT()

TRY
	USE login IN 0 SHARED
FINALLY
	USE					&& close LOGIN if it's open
	SELECT (m.nPreviousArea)
ENDTRY

RETURN .t.
