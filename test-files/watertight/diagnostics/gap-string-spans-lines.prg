* A string literal is not allowed to cross a line break in FoxPro: the tokenizer ends it at the newline. The grammar lets one run on, so a single stray quote swallows every line up to the next quote anywhere in the file.
* This is the worst failure mode in the set because it is silent. The swallowed lines produce no diagnostic of any kind -- not unsupported-syntax, not a block error -- so the linter simply stops seeing code, and the rules downstream report on a file with a hole in it. The IF below is inside the hole, and nothing says so.
* It is also what cost `programs\app\WTMOBILEPROCESS.prg` its parse: a stray apostrophe after the `ENDFOR` at line 3325 ran on for 78 lines and ate the header of the procedure after it, which is why the errors were reported against `SaveInfoFields` and nothing in that procedure explained them. That was the one file SEE-ALSO.md left unattributed.
* The fix belongs in the lexer rather than in any rule: terminate a literal at the end of its line and report the unterminated quote where it opens.
LOCAL m.nTotal
m.nTotal = 0

FOR m.cnt = 1 TO 3
	m.nTotal = m.nTotal + m.cnt
ENDFOR'

IF m.nTotal > 0
	? "this line is invisible to every rule"
ENDIF

? 'the quote here is what finally closes the one above'

RETURN m.nTotal
