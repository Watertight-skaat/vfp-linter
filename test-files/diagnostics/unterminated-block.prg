* Each of these opens a block with no terminator. The grammar's catch-all absorbs the opening
* line instead of failing the parse, so the linter has to report it as an error itself.
IF .T.
	? "no endif"
