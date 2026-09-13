* A string literal is not allowed to cross a line break in FoxPro: the tokenizer ends it at the newline. The grammar let one run on, so a single stray quote swallowed every line up to the next quote anywhere in the file.
* This was the worst failure mode in the set because it was silent. The swallowed lines produced no diagnostic of any kind -- not unsupported-syntax, not a block error -- so the linter simply stopped seeing code and the rules downstream reported on a file with a hole in it. The IF below was inside the hole, and nothing said so.
* It is also what cost `programs\app\WTMOBILEPROCESS.prg` its parse: a stray apostrophe after the `ENDFOR` at line 3325 ran on for 78 lines and ate the header of the procedure after it, which is why the errors were reported against `SaveInfoFields` and nothing in that procedure explained them. That was the one file SEE-ALSO.md left unattributed.
* The fix was in the lexer rather than in any rule: a literal ends at the end of its line, and the quote that opens an unclosed one is reported where it stands. Everything below it is code again, so all this file now carries is that quote and the implicit private the FOR really does create.
LOCAL m.nTotal
m.nTotal = 0

FOR m.cnt = 1 TO 3
	m.nTotal = m.nTotal + m.cnt
ENDFOR'

IF m.nTotal > 0
	? "this line was inside the hole"
ENDIF

? 'this quote is what used to close the one above'

RETURN m.nTotal
